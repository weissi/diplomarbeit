module Data.ExpressionTypes (Expr(..), Operator(..)) where

-- | Arithmetic expressions.
data Expr a = Op Operator (Expr a) (Expr a)
            | Var String
            | Literal a
            deriving (Show)

data Operator = Plus | Minus | Times deriving Show

tryPartialEval :: (Expr a -> Expr a -> Expr a)
               -> (a -> a -> a)
               -> Expr a
               -> Expr a
               -> Expr a
tryPartialEval buildFun evalFun l r =
    case (l, r) of
      (Literal lVal, Literal rVal) -> Literal $ evalFun lVal rVal
      _ -> buildFun l r

instance Num a => Num (Expr a) where
    (+) = tryPartialEval (Op Plus) (+)
    (*) = tryPartialEval (Op Times) (*)
    (-) = tryPartialEval (Op Minus) (-)
    negate a = (Literal 0)-a
    abs = error "abs for instance Num Expr not implemented"
    signum = error "signum for instance Num Expr not implemented"
    fromInteger a = Literal $ fromIntegral a
