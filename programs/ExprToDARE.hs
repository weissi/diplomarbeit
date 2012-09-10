{-# OPTIONS_GHC -Wwarn #-}
module Main where

import Control.Monad.CryptoRandom (runCRand)
import Crypto.Random (SystemRandom, newGenIO)
import Data.List (foldl')
import Math.Algebra.Field.Base (F97)
import qualified Data.DList as DL
import qualified Data.Map as M

import Codec.DARE
import Data.DAREEvaluation
import Data.DARETypes (PrimaryExpression(..), VarMapping)
import Data.ExpressionTypes (Expr(..))
import Data.FieldTypes (Field(..))

import Math.FiniteFields.F2Pow256
type Element = F2Pow256

--import Math.Algebra.Field.Base (Fp)
--import Math.Common.IntegerAsType (IntegerAsType)
--type Element = F97
--instance IntegerAsType n => Field (Fp n) where
--    invert n =
--        case n of
--          0 -> error "0 is not invertible"
--          n' -> 1 / n'



main :: IO ()
main =
    do putStrLn "ExprToDARE: START"
       --test
       putStrLn "EXERCISE 2"
       g <- (newGenIO :: IO SystemRandom)
       let (_, dares) = exprToRP g testExpr1
       print $ DL.toList dares
       let (outM, _) = runRP _TestVarMap_ dares
       putStrLn $ "DIRECT EVALUATION: " ++ show outM
       putStrLn "ExprToDARE: done :-)"
       let erp = prepareRPEvaluation dares
       print erp
       case runERP erp _TestVarMap_ of
         Left err -> putStrLn $ "ERROR: " ++ err
         Right val -> putStrLn $ "OAFE EVALUATION: SUCCESS: " ++ show val
       print $ (map fromInteger [1..10] :: [Element])
       let a :: Element
           a = 345234985
           b = a+a
           c = b + ((-1) * a)
       putStrLn $ "a = " ++ show a
       putStrLn $ "b = a + a = " ++ show b
       putStrLn $ "c = b + ((-1)*a) = a = " ++ show c
       return ()

testExpr1 :: Expr Element
--testExpr1 = _X_ * _X_
--testExpr1 = 4 * _X_ + _Y_ + _X_ * _X_ * _X_
--testExpr1 = _X_ * _Y_
--testExpr1 = _X_ ^ 1000
--testExpr1 = foldl' (+) 0 $ take 10000 $ repeat _X_
testExpr1 = 1 + 17 + _X_ + (_X_ + 23)

_X_ :: Field e => Expr e
_X_ = Var "x"

_Y_ :: Field e => Expr e
_Y_ = Var "y"

_C_1_ :: PrimaryExpression Element
_C_1_ = Constant 1

_C_23_ :: PrimaryExpression Element
_C_23_ = Constant 23

_C_42_ :: PrimaryExpression Element
_C_42_ = Constant 42

_V_x_ :: PrimaryExpression Element
_V_x_ = Variable "x"

_V_y_ :: PrimaryExpression Element
_V_y_ = Variable "y"

_V_z_ :: PrimaryExpression Element
_V_z_ = Variable "z"

_TestVarMap_ :: VarMapping Element
_TestVarMap_ = M.fromList [ ("x", 17), ("y", 23) ]
