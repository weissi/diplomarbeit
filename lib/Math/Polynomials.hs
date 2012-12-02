{-# LANGUAGE ScopedTypeVariables #-}
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
