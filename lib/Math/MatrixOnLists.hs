module Math.MatrixOnLists (add, scale, mult, transpose) where

import Data.List (transpose)

rows :: [[a]] -> Int
rows = length

cols :: [[a]] -> Int
cols m =
    let lenMin = minimum $ map length m
        lenMax = maximum $ map length m
        len = if lenMin == lenMax
                 then lenMin
                 else error $ "matrix shape error: between "++show lenMin
                              ++" and "++show lenMax++" columns"
     in len

shape :: [[a]] -> (Int, Int)
shape m = (rows m, cols m)

add :: Num a => [[a]] -> [[a]] -> [[a]]
add l r = if shape l == shape r
             then unsafeAdd l r
             else error $ "add: matrices of differnt shapes ("
                          ++show (shape l)++" vs. "++show (shape r)++")"

unsafeAdd :: Num a => [[a]] -> [[a]] -> [[a]]
unsafeAdd = zipWith $ zipWith (+)

scale :: Num a => a -> [[a]] -> [[a]]
scale s m = case shape m of
              (_, _) -> unsafeScale s m

unsafeScale :: Num a => a -> [[a]] -> [[a]]
unsafeScale = map . map . (*)

mult :: Num a => [[a]] -> [[a]] -> [[a]]
mult l r =
    let (_, lcol) = shape l
        (rrow, _) = shape r
     in if lcol /= rrow
           then error $ "mult: cannot multiply matrice of shapes "
                        ++show (shape l)++" and "++show (shape r)
           else unsafeMult l r

unsafeMult :: Num a => [[a]] -> [[a]] -> [[a]]
unsafeMult a b = [map (sum . zipWith (*) r) $ transpose b | r <- a]
