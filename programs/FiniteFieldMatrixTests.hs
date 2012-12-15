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

module Main where

import Data.Packed.Matrix ((><), rows, toLists)
import Numeric.Container ((<>))
import qualified Data.Packed.Matrix as HM

import Math.FiniteFields.F2Pow256

instance HM.Element F2Pow256

one :: F2Pow256
one = 1

oneToNine :: [F2Pow256]
oneToNine = map fromInteger [1..]

main :: IO ()
main =
    do putStrLn "BEGIN"
       print one
       let a = (100 >< 100) oneToNine
       print a
       let b = (1 >< 1) [one]
       print $ rows b
       print $ rows a
       let c = HM.trans a
       let d = HM.trans c
       print c
       print $ toLists a == toLists d
       putStrLn "END"
