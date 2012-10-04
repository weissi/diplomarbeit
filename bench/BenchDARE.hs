{-# LANGUAGE OverloadedStrings #-}
module Main where

-- STDLIB
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.List (foldl')
import qualified Data.Map as M

-- SITE PACKAGES
import Control.Monad.CryptoRandom (CRandom(..))
import Crypto.Random (SystemRandom, newGenIO)
import Math.Algebra.Field.Base (Fp, F97)
import Math.Common.IntegerAsType (IntegerAsType)

-- CRITERION
import Criterion.Main
import Criterion.Types

-- LOCAL
import Codec.DARE (exprToRP)
import Data.DAREEvaluation (runRP)
import Data.ExpressionTypes (Expr(..))
import Data.FieldTypes (Field(..))
import Data.LinearExpression (VariableName, VarMapping)
import Math.FiniteFields.F2Pow256 (F2Pow256)

instance IntegerAsType n => Field (Fp n) where
    invert n =
        case n of
          0 -> error "0 is not invertible"
          n' -> 1 / n'
    zero = fromInteger 0
    one = fromInteger 1

instance IntegerAsType n => CRandom (Fp n) where
    crandom g =
        case crandom g of
          Left err -> Left err
          Right (a, g') -> Right (fromIntegral (a :: Int), g')

evalDirect :: (CRandom el, Field el, Show el)
           => SystemRandom -> VarMapping el -> Expr el -> el
evalDirect g varMap expr =
    let (_, dares) = exprToRP g expr
        out = fromJust . fst $ runRP varMap dares
     in out `seq` out

_TVM_F97_ :: Map VariableName F97
_TVM_F97_ = M.fromList [("x", 17), ("y", 23)]

_TVM_F2Pow256_ :: Map VariableName F2Pow256
_TVM_F2Pow256_ = M.fromList [("x", 2^256-1), ("y", 1234)]

_X_ :: Field e => Expr e
_X_ = Var "x"

_Y_ :: Field e => Expr e
_Y_ = Var "y"

main =
    do g <- (newGenIO :: IO SystemRandom)
       defaultMain
         [ bgroup "dare direct F97"
             [ bench "x ^ 1"    $ whnf (evalD97 g) (_X_ ^ 1)
             , bench "x ^ 10"   $ whnf (evalD97 g) (_X_ ^ 10)
             , bench "x ^ 100"  $ whnf (evalD97 g) (_X_ ^ 100)
             , bench "x ^ 1000" $ whnf (evalD97 g) (_X_ ^ 1000)
             , bench "x * 1"    $ whnf (evalD97 g) (addNx 1)
             , bench "x * 10"   $ whnf (evalD97 g) (addNx 10)
             , bench "x * 100"  $ whnf (evalD97 g) (addNx 100)
             , bench "x * 1000" $ whnf (evalD97 g) (addNx 1000)
             , bench "x * 10000"$ whnf (evalD97 g) (addNx 10000)
             ]
         ,  bgroup "dare direct F2Pow256"
             [ bench "x ^ 1"    $ whnf (evalDF2e256 g) (_X_ ^ 1)
             , bench "x ^ 10"   $ whnf (evalDF2e256 g) (_X_ ^ 10)
             , bench "x ^ 100"  $ whnf (evalDF2e256 g) (_X_ ^ 100)
             , bench "x ^ 1000" $ whnf (evalDF2e256 g) (_X_ ^ 1000)
             , bench "x * 1"    $ whnf (evalDF2e256 g) (addNx 1)
             , bench "x * 10"   $ whnf (evalDF2e256 g) (addNx 10)
             , bench "x * 100"  $ whnf (evalDF2e256 g) (addNx 100)
             , bench "x * 1000" $ whnf (evalDF2e256 g) (addNx 1000)
             , bench "x * 10000"$ whnf (evalDF2e256 g) (addNx 10000)
             ]

         ]
    where evalD97 g = evalDirect g _TVM_F97_
          evalDF2e256 g = evalDirect g _TVM_F2Pow256_
          addNx n = foldl' (+) 0 $ take n $ repeat _X_
