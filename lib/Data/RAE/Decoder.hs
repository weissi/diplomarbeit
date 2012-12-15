--------------------------------------------------------------------------------
--  This file is part of diplomarbeit ("Diplomarbeit Johannes Weiß").         --
--                                                                            --
--  diplomarbeit is free software: you can redistribute it and/or modify      --
--  it under the terms of the GNU General Public License as published by      --
--  the Free Software Foundation, either version 3 of the License, or         --
--  (at your option) any later version.                                       --
--                                                                            --
--  diplomarbeit is distributed in the hope that it will be useful,           --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of            --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             --
--  GNU General Public License for more details.                              --
--                                                                            --
--  You should have received a copy of the GNU General Public License         --
--  along with diplomarbeit.  If not, see <http://www.gnu.org/licenses/>.     --
--                                                                            --
--  Copyright 2012, Johannes Weiß                                             --
--------------------------------------------------------------------------------

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
import Data.RAE.Types ( DRAE(..), VarMapping, RadicalTuple(..)
                      , MulTermRadicals(..), DualLinearRadicals(..)
                      )
import qualified Data.LinearExpression as LE

eval2 :: Field el => VarMapping el -> RadicalTuple (LinearExpr el) -> Maybe el
eval2 varMap (RT (le1, le2)) =
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
