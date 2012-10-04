{-# LANGUAGE OverloadedStrings #-}
module Data.DARETypes ( VariableName
                      , VarMapping
                      , PrimaryExpression(..)
                      , DARE(..)
                      , RP, RPStmt
                      , Key, KeyPair, BiLinearExpr, BiKeyPair, BiEncPrimExpr(..)
                      , leftVar, rightVar
                      , _SPECIAL_VAR_OUT_, _SPECIAL_VAR_ADDED_PRE_OUT_
                      , _SPECIAL_VAR_PRE_OUT_
                      ) where

import Data.DList (DList)
import qualified Data.DList as DL
import qualified Data.Text as T

import Data.FieldTypes (Field(..))
import Data.LinearExpression (LinearExpr(..), VarMapping, VariableName)

data PrimaryExpression el = Variable VariableName
                          | Constant el

type Key el = el
type KeyPair el = (Key el, Key el)
type BiLinearExpr el = (LinearExpr el, LinearExpr el)
data BiEncPrimExpr el = BiConst el
                      | BiVar (KeyPair el) VariableName
                      deriving Show

type BiKeyPair el = (KeyPair el, KeyPair el)

data DARE el = DARE !(BiKeyPair el)
                    !(DList (BiLinearExpr el, BiLinearExpr el))
                    !(DList (BiLinearExpr el))

type RPStmt el = (VariableName, DARE el)
type RP el = DList (RPStmt el)

instance (Field el, Show el) => Show (DARE el) where
    show = prettyPrintDARE

leftVar :: VariableName -> VariableName
leftVar v = "__enc_" `T.append` v `T.append` "~_1"

rightVar :: VariableName -> VariableName
rightVar v = "__enc_" `T.append` v `T.append` "~_2"

_SPECIAL_VAR_OUT_ :: VariableName
_SPECIAL_VAR_OUT_ = "out"

_SPECIAL_VAR_ADDED_PRE_OUT_ :: VariableName
_SPECIAL_VAR_ADDED_PRE_OUT_ = "__added_z"

_SPECIAL_VAR_PRE_OUT_ :: VariableName
_SPECIAL_VAR_PRE_OUT_ = "z"

prettyPrintDARE :: (Field el, Show el) => DARE el -> String
prettyPrintDARE (DARE keys muls adds) =
    let showSubList :: Show a => String -> [a] -> String
        showSubList b as =
            case as of
              [] -> ""
              (a:as') -> "\t" ++ b ++ show a ++ "\n" ++ showSubList b as'
     in "DARE " ++ show keys ++ "\n" ++
            showSubList "M: " (DL.toList muls) ++
            showSubList "A: " (DL.toList adds)
