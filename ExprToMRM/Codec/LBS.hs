{-# LANGUAGE BangPatterns #-}
module Codec.LBS ( Expr(Var), InputValues
                 , renderLBSProgram
                 , runLBS, lbsFromExpr, lbsProgramLength
                 , LBSProgram, LBSStmt(..), Register(..), RegisterState
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

data LBSStmt = Offset !Register !OffsetDirection !Register !ScaleFactor

data Register = Reg { getReg :: ! Integer } deriving (Eq)

data OffsetDirection = OffsetPlus | OffsetMinus

data ScaleFactor = ScaleFactorConstant Integer
                 | ScaleFactorInput String

type InputValues = Map String Integer

data Expr = Op Operator Expr Expr
          | Var String
          | Literal Integer
          deriving (Show)

type RegisterState = Map Integer Integer

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

freeRegister :: [Register] -> Register
freeRegister dirtyRegs = Reg (1 + maximum (map getReg dirtyRegs))

-- |Offsets `output register' by +/- `scale register' * (`expr 1' + `expr 2')
lbsFromAddExpr :: [Register]       -- dirty registers
               -> Register         -- output register
               -> OffsetDirection  -- +/-
               -> Register         -- scale register
               -> Expr             -- expr 1
               -> Expr             -- expr 2
               -> LBSProgram
lbsFromAddExpr dirtyRegs regOut direction regScale el er  =
    lbsl `DL.append` lbsr
    where lbsl :: LBSProgram
          lbsl = lbsFromExpr' dirtyRegs regOut direction regScale el
          lbsr :: LBSProgram
          lbsr = lbsFromExpr' dirtyRegs regOut direction regScale er

-- |Offsets `output register' by +/- `scale register' * (`expr 1' * `expr 2')
lbsFromMulExpr :: [Register]       -- dirty registers
               -> Register         -- output register
               -> OffsetDirection  -- +/-
               -> Register         -- scale register
               -> Expr             -- expr 1
               -> Expr             -- expr 2
               -> LBSProgram
lbsFromMulExpr dirtyRegs regOut direction regScale el er =
    case direction of
      OffsetPlus ->
          DL.concat [ lbsFromExpr' dirtyRegs' rk OffsetMinus rj er
                    , lbsFromExpr' dirtyRegs' rj OffsetPlus ri el
                    , lbsFromExpr' dirtyRegs' rk OffsetPlus rj er
                    , lbsFromExpr' dirtyRegs' rj OffsetMinus ri el
                    ]
      OffsetMinus ->
          DL.concat [ lbsFromExpr' dirtyRegs' rk OffsetMinus rj er
                    , lbsFromExpr' dirtyRegs' rj OffsetMinus ri el
                    , lbsFromExpr' dirtyRegs' rk OffsetPlus rj er
                    , lbsFromExpr' dirtyRegs' rj OffsetPlus ri el
                    ]
    where ri = regScale
          rj = freeRegister dirtyRegs
          rk = regOut
          dirtyRegs' = rj : dirtyRegs

-- |Offsets `output register' by +/- `scale register' * `expr'
lbsFromExpr' :: [Register]       -- dirty registers
             -> Register         -- output register
             -> OffsetDirection  -- +/-
             -> Register         -- scale register
             -> Expr             -- expr
             -> LBSProgram
lbsFromExpr' dirtyRegs regOut direction regScale e =
    case e of
      Op o el er ->
          case o of
            Plus -> lbsFromAddExpr dirtyRegs regOut direction regScale el er
            Minus -> undefined
            Times -> lbsFromMulExpr dirtyRegs regOut direction regScale el er
      Var s ->
          DL.singleton $Offset regOut direction regScale (ScaleFactorInput s)
      Literal i ->
          DL.singleton $Offset regOut direction regScale (ScaleFactorConstant i)

lbsFromExpr :: Expr -> LBSProgram
lbsFromExpr e = lbsFromExpr' alwaysDirtyRegs (Reg 1) OffsetPlus _REG_CONST_1_ e
    where alwaysDirtyRegs = [_REG_CONST_1_, Reg 1]

registerValue :: Register -> RegisterState -> Integer
registerValue (Reg r) regs = M.findWithDefault 0 r regs

registerValueM :: Register -> RegisterStateMonad Integer
registerValueM r = get >>= return . (registerValue r)

updateRegisterM :: Register -> Integer -> RegisterStateMonad ()
updateRegisterM (Reg !r) !v = get >>= return . (M.insert r v) >>= put

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

runLBS :: InputValues -> LBSProgram -> Maybe (Integer, RegisterState)
runLBS inputs lbs =
    if null $ DL.toList errs
      then Just $ (registerValue (Reg 1) state, state)
      else Nothing
    where (errs, state) =
              runState (execLBSProgram inputs lbs) _INITIAL_REGISTER_STATE_

lbsProgramLength :: LBSProgram -> Int
lbsProgramLength = length . DL.toList
