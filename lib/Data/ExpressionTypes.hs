module Data.ExpressionTypes (Expr(..), Operator(..)) where

data Expr a = Op Operator (Expr a) (Expr a)
            | Var String
            | Literal a
            deriving (Show)

data Operator = Plus | Minus | Times deriving Show

instance Num a => Num (Expr a) where
    (+) = Op Plus
    (*) = Op Times
    (-) = Op Minus
    negate a = Literal (-1) * a
    abs = error "abs for instance Num Expr not implemented"
    signum = error "signum for instance Num Expr not implemented"
    fromInteger a = Literal $ fromIntegral a
