module ExprToMRM (main) where

import Control.Monad (liftM)
import Control.Monad.State
import Data.DList (DList())
import Data.Map (Map())
import Data.Monoid (mappend)
import Data.Text (Text())
import Data.Text.Lazy.Builder (Builder(), fromString, toLazyText)
import qualified Data.DList as DL
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO

--import Debug.Trace
--traceId :: Show a => a -> a
--traceId a = traceShow a a
--
--traceIdPrep :: Show a => String -> a -> a
--traceIdPrep s a = trace (s ++ show a) a

data Operator = Plus | Minus | Times deriving Show
data Expr = Op Operator Expr Expr
          | Var String
          | Literal Integer
          deriving (Show)

data Register = Reg Integer
type LBSProgram = DL.DList LBSStmt

_REG_CONST_1_ = Reg 0

data OffsetDirection = OffsetPlus | OffsetMinus

data ScaleFactor = ScaleFactorConstant Integer
                 | ScaleFactorInput String

data LBSStmt = Offset Register OffsetDirection Register ScaleFactor

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
                ScaleFactorConstant i -> show i
                ScaleFactorInput i -> i

renderLBSProgram :: LBSProgram -> Builder
renderLBSProgram lbs = DL.foldr f (fromString "") lbs
    where
    f :: LBSStmt -> Builder -> Builder
    f l s = (renderLBSStmt l) `mappend` (fromString "\n") `mappend` s

instance Num Expr where
    (+) l r = Op Plus l r
    (*) l r = Op Times l r
    (-) l r = l + ((Literal (-1)) * r)
    negate a = (Literal (-1)) * a
    abs = id
    signum = id
    fromInteger a = Literal a

nextRegister :: Register -> Register
nextRegister (Reg r) = Reg $ r + 1

zeroRegister :: Register -> LBSStmt
zeroRegister r = Offset r OffsetMinus r (ScaleFactorConstant 1)

lbsFromAddExpr :: Expr -> Expr -> Register -> Register -> LBSProgram
lbsFromAddExpr el er regOut regScale = lbsl `DL.append` lbsr
    where lbsl :: LBSProgram
          lbsl = lbsFromExpr el regOut OffsetPlus regScale
          lbsr :: LBSProgram
          lbsr = lbsFromExpr er regOut OffsetPlus regScale

lbsFromMulExpr :: Expr -> Expr -> Register -> Register -> LBSProgram
lbsFromMulExpr el er regOut regScale =
    DL.concat [ lbsFromExpr er rk OffsetMinus rj
              , lbsFromExpr el rj OffsetPlus ri
              , lbsFromExpr er rk OffsetPlus rj
              , lbsFromExpr el rj OffsetMinus ri
              ]
    where ri = regScale
          rj = nextRegister rk
          rk = regOut

lbsFromExpr :: Expr -> Register -> OffsetDirection -> Register -> LBSProgram
lbsFromExpr e regOut direction regScale =
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
    where erFirstReg = nextRegister regOut

lbsFromExprFull :: Expr -> LBSProgram
lbsFromExprFull e = lbsFromExpr e (Reg 1) OffsetPlus _REG_CONST_1_

maxPlusOne :: Integer -> Integer -> Integer
maxPlusOne l r = if l == r then l + 1 else max l r

type RegisterState = Map Integer Integer
type RegisterStateMonad = State RegisterState
type InputValues = Map String Integer
type ErrorList = DList String
_INITIAL_REGISTER_STATE_ = M.fromList [(0,1)]

registerValue :: Register -> RegisterState -> Integer
registerValue (Reg r) regs = M.findWithDefault 0 r regs

registerValueM :: Register -> RegisterStateMonad Integer
registerValueM r = get >>= return . (registerValue r)

updateRegisterM :: Register -> Integer -> RegisterStateMonad ()
updateRegisterM (Reg r) v = get >>= return . (M.insert r v) >>= put

execLBSStatement :: InputValues -> LBSStmt -> RegisterStateMonad ErrorList
execLBSStatement inputs stmt@(Offset regOut dir regScale sf) =
    do outVal <- registerValueM regOut
       scaleVal <- registerValueM regScale
       case sfVal of
         Left err -> return $ DL.singleton err
         Right val ->
             do updateRegisterM regOut (dirOp dir outVal (scaleVal * val))
                return DL.empty
       where dirOp :: Num a => OffsetDirection -> (a -> a -> a)
             dirOp dir =
                 case dir of
                   OffsetPlus -> (+)
                   OffsetMinus -> (-)
             sfVal :: Either String Integer
             sfVal =
                 case sf of
                   ScaleFactorConstant i -> Right i
                   ScaleFactorInput i ->
                       case M.lookup i inputs of
                         Nothing -> Left $ "input `" ++ i ++ "' undefined"
                         Just x -> Right x

--execLBSProgram :: InputValues -> LBSProgram -> RegisterStateMonad ErrorList
--execLBSProgram inputs p = DL.foldr (flip (>>=)) (return DL.empty) stmts
--    where stmts :: DList (ErrorList -> RegisterStateMonad ErrorList)
--          stmts = (DL.map (execLBSStatement inputs) p)

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

_VARS_ = M.fromList [("x", 17), ("y", 34)]
_X_ = Var "x"
_Y_ = Var "y"

test :: Expr
test = (4 * _X_ * _X_ + 2 * (_X_ + _Y_ * _Y_) * _X_ * _Y_ + 7) * _X_

test2 :: Expr
test2 = 3 + _X_ * _Y_

main :: IO ()
main = do
    putStrLn "Expression:"
    print test
    putStrLn "LBSStmt:"
    TIO.putStrLn $ toLazyText $ renderLBSProgram $ lbsFromExprFull test
    putStrLn "EXEC:"
    print $ runLBS _VARS_ (lbsFromExprFull test)
    print test
