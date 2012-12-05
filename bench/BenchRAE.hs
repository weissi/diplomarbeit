{-# LANGUAGE OverloadedStrings #-}
module Main where

-- STDLIB
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.List (foldl')
import System.Environment (getArgs)
import qualified Data.Map as M

-- SITE PACKAGES
import Control.Monad.CryptoRandom (CRandom(..))
import Crypto.Random (SystemRandom, newGenIO)
import Math.Algebra.Field.Base (Fp, F97)
import Math.Common.IntegerAsType (IntegerAsType)

-- CRITERION
import Criterion.Config
import Criterion.Main
import Criterion.Types

-- LOCAL
import Data.ExpressionTypes (Expr(..))
import Data.FieldTypes (Field(..))
import Data.LinearExpression (VariableName, VarMapping)
import Data.RAE.Encoder (exprToRAC, exprToDRAC)
import Data.RAE.Evaluation (runRAC, runDRAC)
import Math.FiniteFields.F2Pow256 (F2Pow256)
import qualified Math.Polynomials as P

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
    let (_, drac) = exprToDRAC g expr
        out = fromJust . fst $ runDRAC varMap drac
     in out `seq` out

evalViaRAC :: (CRandom el, Field el, Show el)
           => SystemRandom -> VarMapping el -> Expr el -> el
evalViaRAC g varMap expr =
   let (_, rac, oac) = exprToRAC g expr
    in case runRAC rac oac varMap of
         Left s -> error s
         Right out -> out `seq` out

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
       (cfg, _) <- parseArgs defaultConfig defaultOptions =<< getArgs
       defaultMain
         [ bgroup "DRAC direct F97"
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
         ,  bgroup "DRAC direct F2Pow256"
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
         , bgroup "DRAC via RAC F97"
             [ bench "x ^ 1"    $ whnf (evalRAC97 g) (_X_ ^ 1)
             , bench "x ^ 10"   $ whnf (evalRAC97 g) (_X_ ^ 10)
             , bench "x ^ 100"  $ whnf (evalRAC97 g) (_X_ ^ 100)
             , bench "x ^ 1000" $ whnf (evalRAC97 g) (_X_ ^ 1000)
             , bench "x * 1"    $ whnf (evalRAC97 g) (addNx 1)
             , bench "x * 10"   $ whnf (evalRAC97 g) (addNx 10)
             , bench "x * 100"  $ whnf (evalRAC97 g) (addNx 100)
             , bench "x * 1000" $ whnf (evalRAC97 g) (addNx 1000)
             , bench "x * 10000"$ whnf (evalRAC97 g) (addNx 10000)
             ]
         ,  bgroup "DRAC via RAC F2Pow256"
             [ bench "x ^ 1"    $ whnf (evalRACF2e256 g) (_X_ ^ 1)
             , bench "x ^ 10"   $ whnf (evalRACF2e256 g) (_X_ ^ 10)
             , bench "x ^ 100"  $ whnf (evalRACF2e256 g) (_X_ ^ 100)
             , bench "x ^ 1000" $ whnf (evalRACF2e256 g) (_X_ ^ 1000)
             , bench "x * 1"    $ whnf (evalRACF2e256 g) (addNx 1)
             , bench "x * 10"   $ whnf (evalRACF2e256 g) (addNx 10)
             , bench "x * 100"  $ whnf (evalRACF2e256 g) (addNx 100)
             , bench "x * 1000" $ whnf (evalRACF2e256 g) (addNx 1000)
             , bench "x * 10000"$ whnf (evalRACF2e256 g) (addNx 10000)
             ]
         , bgroup "Horner Poly Building F97"
             [ bench "1..100"     $whnf hornerF97 $ replicate 100 17
             , bench "1..1000"    $whnf hornerF97 $ replicate 1000 17
             , bench "1..10000"   $whnf hornerF97 $ replicate 10000 17
             , bench "1..100000"  $whnf hornerF97 $ replicate 100000 17
             ]
         , bgroup "Horner Poly Building F2Pow256"
             [ bench "1..100"    $whnf hornerF2P $ replicate 100 someF2PElem
             , bench "1..1000"   $whnf hornerF2P $ replicate 1000 someF2PElem
             , bench "1..10000"  $whnf hornerF2P $ replicate 10000 someF2PElem
             , bench "1..100000" $whnf hornerF2P $ replicate 100000 someF2PElem
             ]
         ]
    where evalD97 g = evalDirect g _TVM_F97_
          evalDF2e256 g = evalDirect g _TVM_F2Pow256_
          evalRAC97 g = evalViaRAC g _TVM_F97_
          evalRACF2e256 g = evalViaRAC g _TVM_F2Pow256_
          addNx n = foldl' (+) 0 $ take n $ repeat _X_
          hornerF97 = P.horner (17 :: F97)
          hornerF2P = P.horner someF2PElem
          someF2PElem :: F2Pow256
          someF2PElem = read "[1 0 1 1 0 0 1 0 1]"
