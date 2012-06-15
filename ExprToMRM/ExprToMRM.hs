module Main (main) where

import Codec.LBS ( Expr(..), InputValues, lbsFromExpr, renderLBSProgram, runLBS
                 , LBSProgram, LBSStmt(..), Register(..), OffsetDirection(..)
                 , ScaleFactor(..))
import Data.Monoid (mappend, mconcat)
import Data.Text.Lazy (Text())
import Data.Text.Lazy.Builder (Builder(), fromString, toLazyText)
import qualified Data.DList as DL
import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as TIO

_VARS_ :: InputValues
_VARS_ = M.fromList [("x", 17), ("y", 34)]

_X_ :: Expr
_X_ = Var "x"

_Y_ :: Expr
_Y_ = Var "y"

test :: Expr
test = (4 * _X_ * _X_ + 2 * (_X_ + _Y_ * _Y_) * _X_ * _Y_ + 7) * _X_

--test2 :: Expr
--test2 = 3 + _X_ * _Y_

fromShow :: Show a => a -> Builder
fromShow = fromString . show

maxRegister :: LBSStmt -> Integer
maxRegister (Offset (Reg o) _ (Reg i) _) = max i o

maximaMatrixFromLBSStmt :: Integer -> (LBSStmt, Integer) -> Builder
maximaMatrixFromLBSStmt matrixSize (Offset (Reg o) dir (Reg i) sf, lineNo) =
    fromString "M" `mappend`
    fromShow lineNo `mappend`
    fromString " : ident(" `mappend`
    fromShow matrixSize `mappend`
    fromString ");\nM" `mappend`
    fromShow lineNo `mappend`
    fromString "[" `mappend`
    fromShow (o+1) `mappend`
    fromString ", " `mappend`
    fromShow (i+1) `mappend`
    fromString "] : " `mappend`
    fromString dirString `mappend`
    fromString sfString `mappend`
    fromString ";"
    where dirString =
              case dir of
                OffsetPlus -> "+" 
                OffsetMinus -> "-" 
          sfString =
              case sf of
                ScaleFactorConstant c -> show c
                ScaleFactorInput input -> input

maximaMatrixFromLBSProgram :: LBSProgram -> Text
maximaMatrixFromLBSProgram lbs =
    toLazyText $
        foldr joinStmts (fromString "") numberedStmts `mappend`
        (mconcat $  map (\i -> (fromString "M") `mappend` (fromShow i) `mappend`
                               (fromString " . "))
                        (reverse stmtNums))
    where
    numberedStmts :: [(LBSStmt, Integer)]
    numberedStmts = zip (DL.unDL lbs []) [1..]
    stmtNums :: [Integer]
    stmtNums = map snd numberedStmts
    stmts :: [LBSStmt]
    stmts = map fst numberedStmts
    maxReg :: Integer
    maxReg = 1 + (maximum $ map maxRegister stmts)
    joinStmts :: (LBSStmt, Integer) -> Builder -> Builder
    joinStmts l s = (maximaMatrixFromLBSStmt maxReg l) `mappend`
                    (fromString "\n") `mappend`
                    s

main :: IO ()
main = do
    putStrLn "Expression:"
    print test
    putStrLn "LBSStmt:"
    TIO.putStrLn $ renderLBSProgram $ lbsFromExpr test
    putStrLn "EXEC:"
    print $ runLBS _VARS_ (lbsFromExpr test)
    putStrLn "LBSStmt:"
    TIO.putStrLn $ maximaMatrixFromLBSProgram $ lbsFromExpr test
    print test
