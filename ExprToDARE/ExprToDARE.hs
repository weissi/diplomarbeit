module Main where

import qualified Codec.DARE as DARE

main :: IO ()
main =
    do putStrLn "ExprToDARE: START"
       DARE.test
       putStrLn "ExprToDARE: done :-)"
