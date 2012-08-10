module Data.ExpressionTypes (Expr(..), Operator(..)) where

data Expr = Op Operator Expr Expr
          | Var String
          | Literal Integer
          deriving (Show)

data Operator = Plus | Minus | Times deriving Show

instance Num Expr where
    (+) l r = Op Plus l r
    (*) l r = Op Times l r
    (-) l r = l + ((Literal (-1)) * r)
    negate a = (Literal (-1)) * a
    abs = error "abs for instance Num Expr not implemented"
    signum = error "signum for instance Num Expr not implemented"
    fromInteger a = Literal a
