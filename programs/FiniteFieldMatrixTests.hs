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
