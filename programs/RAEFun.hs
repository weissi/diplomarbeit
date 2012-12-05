{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wwarn #-}
module Main where

import Crypto.Random (SystemRandom, newGenIO)
import qualified Data.DList as DL
import qualified Data.Map as M

import Data.ExpressionTypes
import Data.LinearExpression
import Data.FieldTypes (Field(..))
import Data.RAE.Encoder
import Data.RAE.Types
import qualified Math.Polynomials as P


import Math.FiniteFields.F2Pow256
type Element = F2Pow256

--import Math.Algebra.Field.Base (F97, Fp)
--import Math.Common.IntegerAsType (IntegerAsType)
--type Element = F97
--instance IntegerAsType n => Field (Fp n) where
--    invert n =
--        case n of
--          0 -> error "0 is not invertible"
--          n' -> 1 / n'
--
--instance IntegerAsType n => Read (Fp n) where
--    readsPrec _ value = [(fromInteger $ (read value :: Integer), "")]

main :: IO ()
main =
    do putStrLn "RAE Fun: START"
       --test
       putStrLn "EXERCISE 2"
       g <- newGenIO :: IO SystemRandom
       --let (_, drac) = exprToDRAC g testExpr1
       --let (rac, _) = singularizeDRAC drac
       let (_, rac, _) = exprToRAC g testExpr1
       print $ length $ show $ take 1 $ drop 5600 $ rac
       return ()

testExpr1 :: Expr Element
testExpr1 = P.horner _X_ (map (Literal . fromInteger) [1..6000])

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
