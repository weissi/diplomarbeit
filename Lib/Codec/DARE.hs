{-# LANGUAGE ScopedTypeVariables #-}
module Codec.DARE (test
                  , LinearExpr(..)
                  , DARE(..)
                  ) where


import Data.List (foldl')
import Data.Map (Map)
import Control.Monad (liftM, liftM2)
import Control.Monad.CryptoRandom (CRand, runCRand, getCRandom)
import Crypto.Random (newGenIO, GenError, SystemRandom, CryptoRandomGen)
import Math.Algebra.Field.Base (F97)
import qualified Data.Map as M

type Element = F97
type Variable = String
type VarMapping = Map Variable Element

data PrimaryExpression = Var Variable
                       | Const Element

data LinearExpr = LinearExpr { leSlope     :: Element
                             , leVariable  :: Variable
                             , leIntercept :: Element
                             }
                | ConstLinearExpr Element

data DARE = DARE { dareMults :: [(LinearExpr, LinearExpr)]
                 , dareAdds  :: [LinearExpr]
                 } deriving Show

instance Show LinearExpr where
    show le =
        case le of
          LinearExpr s v i -> show s ++ " * " ++ v ++ " + " ++ show i
          ConstLinearExpr cle -> show cle

getRandomElement :: forall g. CryptoRandomGen g => CRand g GenError Element
getRandomElement = liftM fromIntegral (getCRandom :: CRand g GenError Int)

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

-- |DARE for a multiplication getting randoms from generator
dareEncodeMulRnd :: CryptoRandomGen g
                 => PrimaryExpression
                 -> PrimaryExpression
                 -> PrimaryExpression
                 -> CRand g GenError DARE
dareEncodeMulRnd x1 x2 x3 =
    do r1 <- getRandomElement
       r2 <- getRandomElement
       r3 <- getRandomElement
       r4 <- getRandomElement
       return $ dareEncodeMul x1 x2 x3 r1 r2 r3 r4

-- |DARE for an addition getting randoms from generator
dareEncodeAddRnd :: CryptoRandomGen g
                 => PrimaryExpression
                 -> PrimaryExpression
                 -> CRand g GenError DARE
dareEncodeAddRnd x1 x2 =
    do r <- getRandomElement
       return $ dareEncodeAdd x1 x2 r

-- |DARE for an addition f(x1, x2) = x1 + x2
-- see How to Garble Arithmetic Circuits, p. 13
dareEncodeAdd :: PrimaryExpression
              -> PrimaryExpression
              -> Element
              -> DARE
dareEncodeAdd x1 x2 r =
    let le1 = genLinearExpr 1 x1 r
        le2 = genLinearExpr 1 x2 (-r)
        in DARE [] [le1, le2]

-- |DARE for a multiplication (+ addition) f(x1, x2, x3) = x1 * x2 + x3
-- see How to Garble Arithmetic Circuits, p. 13
dareEncodeMul :: PrimaryExpression
              -> PrimaryExpression
              -> PrimaryExpression
              -> Element
              -> Element
              -> Element
              -> Element
              -> DARE
dareEncodeMul x1 x2 x3 r1 r2 r3 r4 =
    let le1 = genLinearExpr 1 x1 (-r1)
        le2 = genLinearExpr r2 x1 (-r1*r2 + r3)
        le3 = genLinearExpr 1 x2 (-r2)
        le4 = genLinearExpr r1 x2 r4
        le5 = genLinearExpr 1 x3 (-r3-r4)
        in DARE [(le1, le3)] [le2, le4, le5]

addToLinearExpression :: LinearExpr
                      -> Element
                      -> LinearExpr
addToLinearExpression le e =
    case le of
      ConstLinearExpr c -> ConstLinearExpr (c + e)
      LinearExpr s v i -> LinearExpr s v (i + e)

addToDARE :: DARE
          -> Element
          -> DARE
addToDARE (DARE muls adds) e =
    case adds of
      [] -> DARE muls [ConstLinearExpr e]
      (x:xs) -> DARE muls ((addToLinearExpression x e):xs)

-- |DARE for an addition of two DAREs
dareEncodeDareAdd :: DARE
                  -> DARE
                  -> Element
                  -> DARE
dareEncodeDareAdd dl dr r =
    DARE (dlMuls' ++ drMuls') (dlAdds' ++ drAdds')
    where DARE dlMuls' dlAdds' = addToDARE dl (-r)
          DARE drMuls' drAdds' = addToDARE dr r

-- |DARE for a DARE addition getting randoms from generator
dareEncodeDareAddRnd :: CryptoRandomGen g
                     => DARE
                     -> DARE
                     -> CRand g GenError DARE
dareEncodeDareAddRnd dl dr =
    do r <- getRandomElement
       return (dareEncodeDareAdd dl dr r)

-- |DARE decoder
dareDecode :: VarMapping -> DARE -> Maybe Element
dareDecode varMap (DARE muls adds) =
    case sequence (addValsM ++ mulValsM) of
      Nothing -> Nothing
      Just vals -> Just $ foldl' (+) 0 vals
    where addValsM :: [Maybe Element]
          addValsM = map (calcLinearExpr varMap) adds
          mulValsM :: [Maybe Element]
          mulValsM = map doMul muls
          doMul :: (LinearExpr, LinearExpr) -> Maybe Element
          doMul (lel, ler) = liftM2 (*) (calcLinearExpr varMap lel)
                                        (calcLinearExpr varMap ler)


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
                 lesOut <- dareEncodeDareAddRnd les1 les2
                 return lesOut
