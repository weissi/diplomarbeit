{-# LANGUAGE ScopedTypeVariables #-}
module Data.RAE.Decoder (decodeDRAE) where

-- # STDLIB
import Control.Monad (liftM2)
import Data.List (foldl')

-- # SITE PACKAGES
import qualified Data.DList as DL

-- # LOCAL
import Data.FieldTypes (Field)
import Data.LinearExpression (LinearExpr(..))
import Data.RAE.Types (DRAE(..), VarMapping)
import qualified Data.LinearExpression as LE

-- |DRAE decoder
decodeDRAE :: forall el. Field el
           => VarMapping el
           -> DRAE el
           -> Maybe (el, el)
decodeDRAE varMap (DRAE _ muls adds) =
    do l <- outL
       r <- outR
       return (l, r)
    where addValsM :: ((LinearExpr el, LinearExpr el) -> LinearExpr el)
                   -> [Maybe el]
          addValsM prj = map (LE.evaluate varMap . prj) (DL.toList adds)
          mulValsM :: ((LinearExpr el, LinearExpr el) -> LinearExpr el)
                   -> [Maybe el]
          mulValsM prj = map (doMul prj) $ DL.toList muls
          doMul prj (lel, ler) = liftM2 (*) (LE.evaluate varMap (prj lel))
                                        (LE.evaluate varMap (prj ler))
          outL = case sequence (addValsM fst ++ mulValsM fst) of
                   Nothing -> Nothing
                   Just vals -> Just $ foldl' (+) 0 vals
          outR = case sequence (addValsM snd ++ mulValsM snd) of
                   Nothing -> Nothing
                   Just vals -> Just $ foldl' (+) 0 vals
