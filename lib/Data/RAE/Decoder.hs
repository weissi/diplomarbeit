{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
-- | This module contains direct DRAE decoding.
--
-- That's not needed in the real-world. Only for tests/benchmarks.
module Data.RAE.Decoder (decodeDRAE) where

-- # STDLIB
import Control.Monad (liftM2)
import Data.List (foldl')

-- # SITE PACKAGES
import qualified Data.DList as DL

-- # LOCAL
import Data.FieldTypes (Field)
import Data.LinearExpression (LinearExpr(..))
import Data.RAE.Types ( DRAE(..), VarMapping, LinearRadicals(..)
                      , MulTermRadicals(..), DualLinearRadicals(..)
                      )
import qualified Data.LinearExpression as LE

eval2 :: Field el => VarMapping el -> LinearRadicals el -> Maybe el
eval2 varMap (LinearRadicals (le1, le2)) =
    do v1 <- LE.evaluate varMap le1
       v2 <- LE.evaluate varMap le2
       return $! v1 + v2

-- |DRAE decoder
--
-- Only used internally.
decodeDRAE :: forall el. Field el
           => VarMapping el
           -> DRAE el
           -> Maybe (el, el)
decodeDRAE varMap (DRAE _ muls adds) =
    do l <- outL
       r <- outR
       return (l, r)
    where addValsM :: ((LinearExpr el, LinearExpr el) -> LinearExpr el)
                   -> [Maybe el]
          addValsM prj = map (LE.evaluate varMap . prj) (DL.toList adds)
          mulValsM :: (MulTermRadicals el -> DualLinearRadicals el)
                   -> [Maybe el]
          mulValsM prj = map (doMul . prj) (DL.toList muls)
          doMul (DLR (lrl, lrr)) =
              liftM2 (*) (eval2 varMap lrl) (eval2 varMap lrr)
          outL = case sequence (addValsM fst ++ mulValsM mtrLeft) of
                   Nothing -> Nothing
                   Just vals -> Just $ foldl' (+) 0 vals
          outR = case sequence (addValsM snd ++ mulValsM mtrRight) of
                   Nothing -> Nothing
                   Just vals -> Just $ foldl' (+) 0 vals
