module Math.MatrixOnLists ( safeCols, safeRows, safeShape
                          , safeAdd, safeScale, safeMult, safeTranspose
                          , cols, rows, shape
                          , add, scale, mult, transpose
                          ) where

import Data.List (transpose)

safeTranspose :: [[a]] -> Maybe [[a]]
safeTranspose m =
    do _ <- safeShape m
       Just $ transpose m

rows :: [[a]] -> Int
rows = length

safeRows :: [[a]] -> Maybe Int
safeRows = return . rows

cols :: [[a]] -> Int
cols m =
    case safeCols m of
      Just a -> a
      Nothing -> error $ "cols: matrix shape error differnt number of columns"

safeCols :: [[a]] -> Maybe Int
safeCols m =
    let lenMin = minimum $ map length m
        lenMax = maximum $ map length m
     in if lenMin == lenMax
           then Just lenMin
           else Nothing

shape :: [[a]] -> (Int, Int)
shape m = (rows m, cols m)

safeShape :: [[a]] -> Maybe (Int, Int)
safeShape m =
    do r <- safeRows m
       c <- safeCols m
       return (r, c)

add :: Num a => [[a]] -> [[a]] -> [[a]]
add l r =
    case safeAdd l r of
      Just o -> o
      Nothing -> error $ "add: matrices of differnt shapes"

safeAdd :: Num a => [[a]] -> [[a]] -> Maybe [[a]]
safeAdd l r =
    do sl <- safeShape l
       sr <- safeShape r
       if sl == sr
          then Just $ unsafeAdd l r
          else Nothing

unsafeAdd :: Num a => [[a]] -> [[a]] -> [[a]]
unsafeAdd = zipWith $ zipWith (+)

scale :: Num a => a -> [[a]] -> [[a]]
scale s m = case safeScale s m of
              Just o -> o
              Nothing -> error "error: The impossible happened"

safeScale :: Num a => a -> [[a]] -> Maybe [[a]]
safeScale s m =
    do _ <- safeShape m
       Just $ unsafeScale s m

unsafeScale :: Num a => a -> [[a]] -> [[a]]
unsafeScale = map . map . (*)

mult :: Num a => [[a]] -> [[a]] -> [[a]]
mult l r =
    case safeMult l r of
        Nothing -> error $ "mult: matrix shape error"
        Just o -> o

safeMult :: Num a => [[a]] -> [[a]] -> Maybe [[a]]
safeMult l r =
    do (_, lcol) <- safeShape l
       (rrow, _) <- safeShape r
       if lcol /= rrow
          then Nothing
          else Just $ unsafeMult l r

unsafeMult :: Num a => [[a]] -> [[a]] -> [[a]]
unsafeMult a b = [map (sum . zipWith (*) r) $ transpose b | r <- a]
