module Data.DARETypes ( VariableName
                      , VarMapping
                      , PrimaryExpression(..)
                      , LinearExpr(..)
                      , DARE(..)
                      , RP, RPStmt
                      , mulElementToLinearExpression
                      ) where

import Data.DList (DList)
import Data.Map (Map)

import Data.FieldTypes (FieldElement(..))

type VariableName = String
type VarMapping el = Map VariableName el

data PrimaryExpression el = Variable VariableName
                          | Constant el

data LinearExpr el = LinearExpr { leSlope     :: el
                                , leVariable  :: VariableName
                                , leIntercept :: el
                                }
                   | ConstLinearExpr el

data DARE el = DARE [(LinearExpr el, LinearExpr el)]
                    [LinearExpr el]
                    deriving Show

type RPStmt el = (VariableName, DARE el)
type RP el = DList (RPStmt el)

mulElementToLinearExpression :: FieldElement el
                             => LinearExpr el
                             -> el
                             -> LinearExpr el
mulElementToLinearExpression le el =
    case le of
      ConstLinearExpr c -> ConstLinearExpr (c * el)
      LinearExpr s v i -> LinearExpr (s * el) v (i * el)

instance (Show el, FieldElement el) => Show (LinearExpr el) where
    show le =
        case le of
          LinearExpr s v i -> show s ++ " * " ++ v ++ " + " ++ show i
          ConstLinearExpr cle -> show cle

