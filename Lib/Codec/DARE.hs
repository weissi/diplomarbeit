{-# LANGUAGE ScopedTypeVariables #-}
module Codec.DARE ( LinearExpr(..)
                  , DARE
                  , PrimaryExpression (..)
                  , VarMapping
                  , dareEncodeMulRnd
                  , dareEncodeAddRnd
                  , dareEncodeDareAddRnd
                  , dareEncodePrimaryExpr
                  , dareDecode
                  ) where


import Data.List (foldl')
import Data.Map (Map)
import Control.Monad (liftM, liftM2)
import Control.Monad.CryptoRandom (CRand, getCRandom)
import Crypto.Random (GenError, CryptoRandomGen)
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

data DARE = DARE [(LinearExpr, LinearExpr)] [LinearExpr] deriving Show

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
    let dareL = dareEncodePrimaryExpr x1
        dareR = dareEncodePrimaryExpr x2
        in dareEncodeDareAdd dareL dareR r

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

-- |DARE encode constatnt
dareEncodePrimaryExpr :: PrimaryExpression -> DARE
dareEncodePrimaryExpr c = DARE [] [genLinearExpr 1 c 0]

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
