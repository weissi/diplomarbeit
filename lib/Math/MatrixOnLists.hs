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

module Math.MatrixOnLists ( safeCols, safeRows, safeShape
                          , safeAdd, safeScale, safeMult, safeTranspose
                          , cols, rows, shape
                          , add, scale, mult, transpose
                          ) where

import Data.List (transpose)
import Data.Maybe (fromMaybe)

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
    fromMaybe (error "cols: matrix shape error differnt number of columns")
              (safeCols m)

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
add l r = fromMaybe (error "add: matrices of differnt shapes") (safeAdd l r)

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
scale s m = fromMaybe (error "error: The impossible happened") (safeScale s m)

safeScale :: Num a => a -> [[a]] -> Maybe [[a]]
safeScale s m =
    do _ <- safeShape m
       Just $ unsafeScale s m

unsafeScale :: Num a => a -> [[a]] -> [[a]]
unsafeScale = map . map . (*)

mult :: Num a => [[a]] -> [[a]] -> [[a]]
mult l r = fromMaybe (error "mult: matrix shape error") (safeMult l r)

safeMult :: Num a => [[a]] -> [[a]] -> Maybe [[a]]
safeMult l r =
    do (_, lcol) <- safeShape l
       (rrow, _) <- safeShape r
       if lcol /= rrow
          then Nothing
          else Just $ unsafeMult l r

unsafeMult :: Num a => [[a]] -> [[a]] -> [[a]]
unsafeMult a b = [map (sum . zipWith (*) r) $ transpose b | r <- a]
