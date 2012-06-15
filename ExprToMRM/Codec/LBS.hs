module Codec.LBS ( Expr(Var), InputValues
                 , renderLBSProgram
                 , runLBS, lbsFromExpr
                 , LBSProgram, LBSStmt(..), Register(..)
                 , OffsetDirection(..), ScaleFactor(..)
                 ) where

import Control.Monad.State (State, get, put, runState)
import Data.DList (DList())
import Data.Map (Map())
import Data.Monoid (mappend)
import Data.Text.Lazy (Text())
import Data.Text.Lazy.Builder (Builder(), fromString, toLazyText)
import qualified Data.DList as DL
import qualified Data.Map as M

-- Public Data Types
type LBSProgram = DL.DList LBSStmt

data LBSStmt = Offset Register OffsetDirection Register ScaleFactor

data Register = Reg Integer

data OffsetDirection = OffsetPlus | OffsetMinus

data ScaleFactor = ScaleFactorConstant Integer
                 | ScaleFactorInput String

type InputValues = Map String Integer

data Expr = Op Operator Expr Expr
          | Var String
          | Literal Integer
          deriving (Show)

-- Internal Data Types
data Operator = Plus | Minus | Times deriving Show

instance Num Expr where
    (+) l r = Op Plus l r
    (*) l r = Op Times l r
    (-) l r = l + ((Literal (-1)) * r)
    negate a = (Literal (-1)) * a
    abs = error "abs for instance Num Expr not implemented"
    signum = error "signum for instance Num Expr not implemented"
    fromInteger a = Literal a

type RegisterState = Map Integer Integer

type RegisterStateMonad = State RegisterState

type ErrorList = DList String

-- Internal Constants
_REG_CONST_1_ :: Register
_REG_CONST_1_ = Reg 0

_INITIAL_REGISTER_STATE_ :: RegisterState
_INITIAL_REGISTER_STATE_ = M.fromList [(0,1)]

fromShow :: Show a => a -> Builder
fromShow a = fromString $ show a

renderLBSStmt :: LBSStmt -> Builder
renderLBSStmt (Offset (Reg o) dir (Reg i) sf) =
    fromString "R" `mappend`
    fromShow o `mappend`
    fromString " <- R" `mappend`
    fromShow o `mappend`
    fromString " " `mappend`
    fromString dirString `mappend`
    fromString " R" `mappend`
    fromShow i `mappend`
    fromString " * " `mappend`
    fromString sfString
    where dirString =
              case dir of
                OffsetPlus -> "+"
                OffsetMinus -> "-"
          sfString =
              case sf of
                ScaleFactorConstant c -> show c
                ScaleFactorInput input -> input

renderLBSProgram :: LBSProgram -> Text
renderLBSProgram lbs = toLazyText $ DL.foldr joinStmts (fromString "") lbs
    where
    joinStmts :: LBSStmt -> Builder -> Builder
    joinStmts l s = (renderLBSStmt l) `mappend` (fromString "\n") `mappend` s

nextRegister :: Register -> Register
nextRegister (Reg r) = Reg $ r + 1

lbsFromAddExpr :: Expr -> Expr -> Register -> Register -> LBSProgram
lbsFromAddExpr el er regOut regScale = lbsl `DL.append` lbsr
    where lbsl :: LBSProgram
          lbsl = lbsFromExpr' el regOut OffsetPlus regScale
          lbsr :: LBSProgram
          lbsr = lbsFromExpr' er regOut OffsetPlus regScale

lbsFromMulExpr :: Expr -> Expr -> Register -> Register -> LBSProgram
lbsFromMulExpr el er regOut regScale =
    DL.concat [ lbsFromExpr' er rk OffsetMinus rj
              , lbsFromExpr' el rj OffsetPlus ri
              , lbsFromExpr' er rk OffsetPlus rj
              , lbsFromExpr' el rj OffsetMinus ri
              ]
    where ri = regScale
          rj = nextRegister rk
          rk = regOut

lbsFromExpr' :: Expr -> Register -> OffsetDirection -> Register -> LBSProgram
lbsFromExpr' e regOut direction regScale =
    case e of
      Op o el er ->
          case o of
            Plus -> lbsFromAddExpr el er regOut _REG_CONST_1_
            Minus -> undefined
            Times -> lbsFromMulExpr el er regOut _REG_CONST_1_
      Var s ->
          DL.singleton $Offset regOut direction regScale (ScaleFactorInput s)
      Literal i ->
          DL.singleton $Offset regOut direction regScale (ScaleFactorConstant i)

lbsFromExpr :: Expr -> LBSProgram
lbsFromExpr e = lbsFromExpr' e (Reg 1) OffsetPlus _REG_CONST_1_

registerValue :: Register -> RegisterState -> Integer
registerValue (Reg r) regs = M.findWithDefault 0 r regs

registerValueM :: Register -> RegisterStateMonad Integer
registerValueM r = get >>= return . (registerValue r)

updateRegisterM :: Register -> Integer -> RegisterStateMonad ()
updateRegisterM (Reg r) v = get >>= return . (M.insert r v) >>= put

dirOp :: Num a => OffsetDirection -> (a -> a -> a)
dirOp dir = case dir of
              OffsetPlus -> (+)
              OffsetMinus -> (-)

execLBSStatement :: InputValues -> LBSStmt -> RegisterStateMonad ErrorList
execLBSStatement inputs (Offset regOut dir regScale sf) =
    do outVal <- registerValueM regOut
       scaleVal <- registerValueM regScale
       case sfVal of
         Left err -> return $ DL.singleton err
         Right val ->
             do updateRegisterM regOut (dirOp dir outVal (scaleVal * val))
                return DL.empty
       where sfVal :: Either String Integer
             sfVal =
                 case sf of
                   ScaleFactorConstant i -> Right i
                   ScaleFactorInput i ->
                       case M.lookup i inputs of
                         Nothing -> Left $ "input `" ++ i ++ "' undefined"
                         Just x -> Right x

execLBSProgram :: InputValues -> LBSProgram -> RegisterStateMonad ErrorList
execLBSProgram inputs p = sequence (DL.unDL stmts []) >>= return . DL.concat
    where stmts :: DList (RegisterStateMonad ErrorList)
          stmts = (DL.map (execLBSStatement inputs) p)

runLBS :: InputValues -> LBSProgram -> Maybe Integer
runLBS inputs lbs =
    if null $ DL.toList errs
      then Just $ registerValue (Reg 1) state
      else Nothing
    where (errs, state) =
              runState (execLBSProgram inputs lbs) _INITIAL_REGISTER_STATE_
