module Main where

import Codec.DARE
import Control.Monad.CryptoRandom (runCRand)
import Crypto.Random (SystemRandom, newGenIO)
import qualified Data.Map as M

main :: IO ()
main =
    do putStrLn "ExprToDARE: START"
       test
       putStrLn "ExprToDARE: done :-)"

_C_1_ :: PrimaryExpression
_C_1_ = Const 1

_C_23_ :: PrimaryExpression
_C_23_ = Const 23

_C_42_ :: PrimaryExpression
_C_42_ = Const 42

_V_x_ :: PrimaryExpression
_V_x_ = Var "x"

_V_y_ :: PrimaryExpression
_V_y_ = Var "y"

_V_z_ :: PrimaryExpression
_V_z_ = Var "z"

_TestVarMap_ :: VarMapping
_TestVarMap_ = M.fromList [("x", 17), ("y", 23), ("z", 42)]

test :: IO ()
test =
    do g <- (newGenIO :: IO SystemRandom)
       case runCRand testDARE g of
         Right (dare, _) ->
            do print $ dare
               print $ dareDecode _TestVarMap_ dare
         Left _ -> print "left"
       case runCRand (dareEncodeAddRnd _V_y_ _V_x_) g of
         Right (les, _) ->
            do print $ les
               print $ dareDecode _TestVarMap_ les
         Left _ -> print "left"
    where testDARE =
              do les1 <- dareEncodeMulRnd _V_x_ _C_23_ _C_42_
                 les2 <- dareEncodeAddRnd _V_x_ _C_23_
                 let les3 = dareEncodePrimaryExpr _C_1_
                 les4 <- dareEncodeDareAddRnd les1 les2
                 lesOut <- dareEncodeDareAddRnd les3 les4
                 return lesOut
