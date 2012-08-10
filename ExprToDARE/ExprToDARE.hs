module Main where

import Codec.DARE
import Control.Monad.CryptoRandom (runCRand)
import Crypto.Random (SystemRandom, newGenIO)
import Data.ExpressionTypes (Expr(..))
import Math.Algebra.Field.Base (F97)
import qualified Data.DList as DL
import qualified Data.Map as M

main :: IO ()
main =
    do putStrLn "ExprToDARE: START"
       test
       putStrLn "EXERCISE 2"
       g <- (newGenIO :: IO SystemRandom)
       let (_, dares) = exprToRP g testExpr1
       print $ DL.toList dares
       print $ runRP _TestVarMap_ dares
       putStrLn "ExprToDARE: done :-)"

type Element = F97

testExpr1 :: Expr Element
testExpr1 = 4 * _X_ + _Y_ + _X_ * _X_ * _X_
--testExpr1 = (4 * _X_ * _X_ + 2 * (_X_ + _Y_ * (_X_ + _Y_)) * _X_ * _Y_ + 7) * _X_

_X_ :: FieldElement e => Expr e
_X_ = Var "x"

_Y_ :: FieldElement e => Expr e
_Y_ = Var "y"

_C_1_ :: PrimaryExpression Element
_C_1_ = Constant 1

_C_23_ :: PrimaryExpression Element
_C_23_ = Constant 23

_C_42_ :: PrimaryExpression Element
_C_42_ = Constant 42

_V_x_ :: PrimaryExpression Element
_V_x_ = Variable "x"

_V_y_ :: PrimaryExpression Element
_V_y_ = Variable "y"

_V_z_ :: PrimaryExpression Element
_V_z_ = Variable "z"

_TestVarMap_ :: VarMapping Element
_TestVarMap_ = M.fromList [("x", 17), ("y", 23)]

test :: IO ()
test =
    do g1 <- (newGenIO :: IO SystemRandom)
       case runCRand testDARE g1 of
         Right (dare, _) ->
            do print $ dare
               print $ dareDecode _TestVarMap_ dare
         Left _ -> print "left"
       g2 <- (newGenIO :: IO SystemRandom)
       case runCRand (dareEncodeAddRnd _V_y_ _V_x_) g2 of
         Right (les, _) ->
            do print $ les
               print $ dareDecode _TestVarMap_ les
         Left _ -> print "left"
    where testDARE =
              do les1 <- dareEncodeMulRnd _V_x_ _C_42_ _C_42_
                 les2 <- dareEncodeAddRnd _V_x_ _C_23_
                 let les3 = dareEncodePrimaryExpr _C_1_
                 les4 <- dareEncodeDareAddRnd les1 les2
                 lesOut <- dareEncodeDareAddRnd les3 les4
                 return lesOut
