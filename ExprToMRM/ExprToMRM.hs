module Main (main) where

import Codec.LBS ( InputValues, lbsFromExpr, renderLBSProgram, runLBS
                 , LBSProgram, LBSStmt(..), Register(..), OffsetDirection(..)
                 , ScaleFactor(..), lbsProgramLength
                 )
import Data.ExpressionTypes (Expr(..))
import Data.Monoid (mappend, mconcat)
import Data.Text.Lazy (Text())
import Data.Text.Lazy.Builder (Builder(), fromString, toLazyText)
import qualified Data.DList as DL
import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as TIO

_VARS_ :: InputValues
_VARS_ = M.fromList [("x", 17), ("y", 34)]

_X_ :: Expr Integer
_X_ = Var "x"

_Y_ :: Expr Integer
_Y_ = Var "y"

--test :: Expr Integer
--test = (4 * _X_ * _X_ + 2 * (_X_ + _Y_ * _Y_) * _X_ * _Y_ + 7) * _X_

--big_sum :: Expr Integer
--big_sum = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + _X_ + _Y_
--
--test :: Expr Integer
--test = big_sum * big_sum * big_sum * big_sum * big_sum +
--       big_sum * big_sum * big_sum * big_sum * big_sum +
--       big_sum * big_sum * big_sum * big_sum * big_sum
--
--test :: Expr Integer
--test = 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19

--test :: Expr Integer
--test = 3 * _X_  +  4 * _Y_

--pow :: Num a => a -> Integer -> a
--pow a p = a ^ p

t :: Expr Integer
t = ((1 * 2) * (3 * 4)) * ((5 * 6) * (7 * 8)) *
    ((1 * 2) * (3 * 4)) * ((5 * 6) * (7 * 8))

test :: Expr Integer -- size 255
test = ((t * t) * (t * t)) * ((t * t) * (t * t))

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

doLBS :: Expr Integer -> IO ()
doLBS e = do
    putStrLn "Expression:"
    print e
    putStrLn "LBSStmt:"
    TIO.putStrLn $ renderLBSProgram $ lbsFromExpr e
    putStrLn "EXEC:"
    print $ runLBS _VARS_ (lbsFromExpr e)
    putStrLn "LBSStmt:"
    TIO.putStrLn $ maximaMatrixFromLBSProgram $ lbsFromExpr e

main :: IO ()
main = do
    doLBS 1
    print $ lbsProgramLength $ lbsFromExpr test
    loop 1 14 1
    where loop :: Int -> Int -> Expr Integer -> IO ()
          loop i end frag =
              if i < end
                then let lbs = lbsFromExpr e'
                         e' = frag * frag
                     in do
                         putStr $ show i ++ ", "
                         print $ lbsProgramLength lbs
                         print $ runLBS _VARS_ lbs
                         --withFile "/dev/null" WriteMode (\h -> TIO.hPutStr h (renderLBSProgram lbs))
                         loop (i+1) end e'
                else do
                    return ()
