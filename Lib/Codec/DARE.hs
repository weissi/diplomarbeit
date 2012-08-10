{-# LANGUAGE ScopedTypeVariables #-}
module Codec.DARE ( LinearExpr(..)
                  , DARE
                  , PrimaryExpression (..)
                  , FieldElement (..)
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
import Math.Algebra.Field.Base (Fp)
import Math.Common.IntegerAsType (IntegerAsType)
import qualified Data.Map as M

instance IntegerAsType n => FieldElement (Fp n) where
    invert = error "not implemented"

class Num e => FieldElement e where
    invert :: e -> e

type VariableName = String
type VarMapping e = Map VariableName e

data PrimaryExpression e = Variable VariableName
                         | Constant e

data LinearExpr e = LinearExpr { leSlope     :: e
                               , leVariable  :: VariableName
                               , leIntercept :: e
                               }
                  | ConstLinearExpr e

data DARE e = DARE [(LinearExpr e, LinearExpr e)] [LinearExpr e] deriving Show

instance (Show e, FieldElement e) => Show (LinearExpr e) where
    show le =
        case le of
          LinearExpr s v i -> show s ++ " * " ++ v ++ " + " ++ show i
          ConstLinearExpr cle -> show cle

getRandomElement :: forall g e. (CryptoRandomGen g, FieldElement e)
                 => CRand g GenError e
getRandomElement = liftM fromIntegral (getCRandom :: CRand g GenError Int)

genLinearExpr :: FieldElement e => e -> PrimaryExpression e -> e -> LinearExpr e
genLinearExpr a b c =
    case b of
      Variable b' -> LinearExpr { leSlope = a
                                , leVariable = b'
                                , leIntercept = c
                                }
      Constant b' -> ConstLinearExpr (a * b' + c)

calcLinearExpr :: FieldElement e => VarMapping e -> LinearExpr e -> Maybe e
calcLinearExpr varMap le =
    case le of
      ConstLinearExpr cle -> Just cle
      LinearExpr s v i ->
          do vVal <- M.lookup v varMap
             return $ s * vVal + i

-- |DARE for a multiplication getting randoms from generator
dareEncodeMulRnd :: (CryptoRandomGen g, FieldElement e)
                 => PrimaryExpression e
                 -> PrimaryExpression e
                 -> PrimaryExpression e
                 -> CRand g GenError (DARE e)
dareEncodeMulRnd x1 x2 x3 =
    do r1 <- getRandomElement
       r2 <- getRandomElement
       r3 <- getRandomElement
       r4 <- getRandomElement
       return $ dareEncodeMul x1 x2 x3 r1 r2 r3 r4

-- |DARE for an addition getting randoms from generator
dareEncodeAddRnd :: (CryptoRandomGen g, FieldElement e)
                 => PrimaryExpression e
                 -> PrimaryExpression e
                 -> CRand g GenError (DARE e)
dareEncodeAddRnd x1 x2 =
    do r <- getRandomElement
       return $ dareEncodeAdd x1 x2 r

-- |DARE for an addition f(x1, x2) = x1 + x2
-- see How to Garble Arithmetic Circuits, p. 13
dareEncodeAdd :: FieldElement e
              => PrimaryExpression e
              -> PrimaryExpression e
              -> e
              -> DARE e
dareEncodeAdd x1 x2 r =
    let dareL = dareEncodePrimaryExpr x1
        dareR = dareEncodePrimaryExpr x2
        in dareEncodeDareAdd dareL dareR r

-- |DARE for a multiplication (+ addition) f(x1, x2, x3) = x1 * x2 + x3
-- see How to Garble Arithmetic Circuits, p. 13
dareEncodeMul :: FieldElement e
              => PrimaryExpression e
              -> PrimaryExpression e
              -> PrimaryExpression e
              -> e
              -> e
              -> e
              -> e
              -> DARE e
dareEncodeMul x1 x2 x3 r1 r2 r3 r4 =
    let le1 = genLinearExpr 1 x1 (-r1)
        le2 = genLinearExpr r2 x1 (-r1*r2 + r3)
        le3 = genLinearExpr 1 x2 (-r2)
        le4 = genLinearExpr r1 x2 r4
        le5 = genLinearExpr 1 x3 (-r3-r4)
        in DARE [(le1, le3)] [le2, le4, le5]

addToLinearExpression :: FieldElement e
                      => LinearExpr e
                      -> e
                      -> LinearExpr e
addToLinearExpression le e =
    case le of
      ConstLinearExpr c -> ConstLinearExpr (c + e)
      LinearExpr s v i -> LinearExpr s v (i + e)

addToDARE :: FieldElement e
          => DARE e
          -> e
          -> DARE e
addToDARE (DARE muls adds) e =
    case adds of
      [] -> DARE muls [ConstLinearExpr e]
      (x:xs) -> DARE muls ((addToLinearExpression x e):xs)

-- |DARE for an addition of two DAREs
dareEncodeDareAdd :: FieldElement e
                  => DARE e
                  -> DARE e
                  -> e
                  -> DARE e
dareEncodeDareAdd dl dr r =
    DARE (dlMuls' ++ drMuls') (dlAdds' ++ drAdds')
    where DARE dlMuls' dlAdds' = addToDARE dl (-r)
          DARE drMuls' drAdds' = addToDARE dr r

-- |DARE for a DARE addition getting randoms from generator
dareEncodeDareAddRnd :: (CryptoRandomGen g, FieldElement e)
                     => DARE e
                     -> DARE e
                     -> CRand g GenError (DARE e)
dareEncodeDareAddRnd dl dr =
    do r <- getRandomElement
       return (dareEncodeDareAdd dl dr r)

-- |DARE encode constatnt
dareEncodePrimaryExpr :: FieldElement e => PrimaryExpression e -> DARE e
dareEncodePrimaryExpr c = DARE [] [genLinearExpr 1 c 0]

-- |DARE decoder
dareDecode :: forall e. FieldElement e => VarMapping e -> DARE e -> Maybe e
dareDecode varMap (DARE muls adds) =
    case sequence (addValsM ++ mulValsM) of
      Nothing -> Nothing
      Just vals -> Just $ foldl' (+) 0 vals
    where addValsM :: [Maybe e]
          addValsM = map (calcLinearExpr varMap) adds
          mulValsM :: [Maybe e]
          mulValsM = map doMul muls
          doMul :: (LinearExpr e, LinearExpr e) -> Maybe e
          doMul (lel, ler) = liftM2 (*) (calcLinearExpr varMap lel)
                                        (calcLinearExpr varMap ler)
