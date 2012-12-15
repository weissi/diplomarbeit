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
module Math.Polynomials (monomial, horner) where

import Data.List (foldl')

horner :: (Num a) => a -> [a] -> a
horner x = foldr (\a b -> a + b*x) 0

monomial :: forall a. Num a => a -> [a] -> a
monomial x coeffs =
    let powers = [0..] :: [Integer]
        powersOfX = map (\p -> x^p) powers
        exprTuples :: [(a, a)]
        exprTuples = zip coeffs powersOfX
        exprs :: [a]
        exprs = map (uncurry (*)) exprTuples
     in foldl' (+) (fromInteger 0) exprs
