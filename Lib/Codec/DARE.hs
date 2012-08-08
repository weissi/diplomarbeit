module Codec.DARE (test
                  , LinearExpr(..)
                  ) where


import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Control.Monad (liftM2)
import Control.Monad.CryptoRandom (CRand, runCRand, getCRandom)
import Crypto.Random (newGenIO, GenError, SystemRandom, CryptoRandomGen)
import qualified Data.Map as M

type Element = Int
type Variable = String
type Decoder a = VarMapping -> a -> Maybe Element
type VarMapping = Map Variable Element

data PrimaryExpression = Var Variable
                       | Const Element

data LinearExpr = LinearExpr { leSlope     :: Element
                             , leVariable  :: Variable
                             , leIntercept :: Element
                             }
                | ConstLinearExpr Element

instance Show LinearExpr where
    show le =
        case le of
          LinearExpr s v i -> show s ++ " * " ++ v ++ " + " ++ show i
          ConstLinearExpr cle -> show cle

genLinearExpr :: Element -> PrimaryExpression -> Element -> LinearExpr
genLinearExpr a b c =
    case b of
      Var b' -> LinearExpr { leSlope = a
                               , leVariable = b'
                               , leIntercept = c
                               }
      Const b' -> ConstLinearExpr (a * b' + c)

calcLinearExpr :: VarMapping -> LinearExpr -> Maybe Element
calcLinearExpr varMap le =
    case le of
      ConstLinearExpr cle -> Just cle
      LinearExpr s v i ->
          do vVal <- M.lookup v varMap
             return $ s * vVal + i

mulDecoder :: Decoder [LinearExpr]
mulDecoder varMap les =
    case length les of
      5 -> case (mapMaybe (calcLinearExpr varMap) les) of
             [e1, e2, e3, e4, e5] -> Just $ e1 * e3 + e2 + e4 + e5
             _ -> Nothing
      _ -> Nothing

addDecoder :: Decoder [LinearExpr]
addDecoder varMap les =
    case length les of
      2 -> case (mapMaybe (calcLinearExpr varMap) les) of
             [e1, e2] -> Just $ e1 + e2
             _ -> Nothing
      _ -> Nothing

-- |DARE for a multiplication getting randoms from generator
dareEncodeMulRnd :: CryptoRandomGen g
                 => PrimaryExpression
                 -> PrimaryExpression
                 -> PrimaryExpression
                 -> CRand g GenError [LinearExpr]
dareEncodeMulRnd x1 x2 x3 =
    do r1 <- getCRandom
       r2 <- getCRandom
       r3 <- getCRandom
       r4 <- getCRandom
       return $ dareEncodeMul x1 x2 x3 r1 r2 r3 r4

-- |DARE for an addition getting randoms from generator
dareEncodeAddRnd :: CryptoRandomGen g
                 => PrimaryExpression
                 -> PrimaryExpression
                 -> CRand g GenError [LinearExpr]
dareEncodeAddRnd x1 x2 =
    do r <- getCRandom
       return $ dareEncodeAdd x1 x2 r

-- |DARE for an addition f(x1, x2) = x1 + x2
-- see How to Garble Arithmetic Circuits, p. 13
dareEncodeAdd :: PrimaryExpression
              -> PrimaryExpression
              -> Element
              -> [LinearExpr]
dareEncodeAdd x1 x2 r =
    let le1 = genLinearExpr 1 x1 r
        le2 = genLinearExpr 1 x2 (-r)
        in [le1, le2]

-- |DARE for a multiplication (+ addition) f(x1, x2, x3) = x1 * x2 + x3
-- see How to Garble Arithmetic Circuits, p. 13
dareEncodeMul :: PrimaryExpression
              -> PrimaryExpression
              -> PrimaryExpression
              -> Element
              -> Element
              -> Element
              -> Element
              -> [LinearExpr]
dareEncodeMul x1 x2 x3 r1 r2 r3 r4 =
    let le1 = genLinearExpr 1 x1 (-r1)
        le2 = genLinearExpr r2 x1 (-r1*r2 + r3)
        le3 = genLinearExpr 1 x2 (-r2)
        le4 = genLinearExpr r1 x2 r4
        le5 = genLinearExpr 1 x3 (-r3-r4)
        in [le1, le2, le3, le4, le5]

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
         Right ((dec, les), _) ->
            do print $ les
               print $ dec _TestVarMap_ les
         Left _ -> print "left"
    where testDARE =
              do les1 <- dareEncodeMulRnd _V_x_ _C_23_ _C_42_
                 les2 <- dareEncodeAddRnd _V_x_ _C_23_
                 return (dec, (les1, les2))
                 where dec v (l1, l2) =
                           liftM2 (+) (mulDecoder v l1) (addDecoder v l2)
