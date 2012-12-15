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

{-# LANGUAGE BangPatterns #-}
module Main where

-- # STDLIB
import Control.Monad (when)
import Data.List (partition)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Environment (getArgs)
import qualified Data.Map as M
import qualified Data.Text as T


-- # LOCAL
import Data.Helpers (isOptionArg)
import Functionality.AllInOne (evaluateExpr)
import Functionality.Goliath (readExprFromFile)
import qualified Math.Polynomials as P

import StaticConfiguration

_DEBUG_ :: Bool
_DEBUG_ = False

main :: IO ()
main =
    do putStrLn "AllInOne START"
       rawArgs <- getArgs
       let (optionArgs, args) = partition isOptionArg rawArgs
           (filePath, inputArg) =
               case args of
                 f:i:[] -> (f, i)
                 _ -> error $ "Usage: AllInOne [-h|-m|-q] FILE INPUT-EL"
           logMsg =
               if "-q" `elem` optionArgs
                  then (\_ -> return ())
                  else putStrLn
       buildPoly <- if "-m" `elem` optionArgs
                       then do putStrLn "POLY BUILDING: Monomial"
                               return P.monomial
                       else putStrLn "POLY BUILDING: Horner" >> return P.horner
       let !inputElement = (read inputArg :: Element)
           !varMap = M.fromList [(T.pack "x", inputElement)]
       expr <- readExprFromFile (buildPoly _X_) filePath
       when _DEBUG_ (print expr)
       davidStart <- getCurrentTime
       out <- evaluateExpr varMap expr logMsg
       davidStop <- getCurrentTime
       let sToMs :: Double -> Double
           sToMs = (1000 *)
           calcDiff e a = (truncate . sToMs . realToFrac) (diffUTCTime e a)
           diff :: Integer
           diff = calcDiff davidStop davidStart
       putStrLn $ "David: Exited (running "++show diff++"ms)"
       putStrLn $ "David: DAVID DONE, final result = " ++ show out
       putStrLn "AllInOne DONE"
