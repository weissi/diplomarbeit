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

-- | Build a polynomial using /Horner's rule/.
horner :: (Num a)
       => a   -- ^ The polynomial's variable, such as /x/
       -> [a] -- ^ The polynomial's coefficients.
       -> a   -- ^ Polynomial built using /Horner's rule/.
horner x = foldr (\a b -> a + b*x) 0

-- | Build a polynomial in monomial form.
monomial :: forall a. Num a
       => a   -- ^ The polynomial's variable, such as /x/
       -> [a] -- ^ The polynomial's coefficients.
       -> a   -- ^ Resulting Polynomial
monomial x coeffs =
    let powers = [0..] :: [Integer]
        powersOfX = map (\p -> x^p) powers
        exprTuples :: [(a, a)]
        exprTuples = zip coeffs powersOfX
        exprs :: [a]
        exprs = map (uncurry (*)) exprTuples
     in foldl' (+) (fromInteger 0) exprs
