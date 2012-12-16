--------------------------------------------------------------------------------
--  This file is part of diplomarbeit ("Diplomarbeit Johannes Weiß").         --
--                                                                            --
--  diplomarbeit is free software: you can redistribute it and/or modify      --
--  it under the terms of the GNU General Public License as published by      --
--  the Free Software Foundation, either version 3 of the License, or         --
--  (at your option) any later version.                                       --
--                                                                            --
--  diplomarbeit is distributed in the hope that it will be useful,           --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of            --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             --
--  GNU General Public License for more details.                              --
--                                                                            --
--  You should have received a copy of the GNU General Public License         --
--  along with diplomarbeit.  If not, see <http://www.gnu.org/licenses/>.     --
--                                                                            --
--  Copyright 2012, Johannes Weiß                                             --
--------------------------------------------------------------------------------

module Data.ExpressionTypes (Expr(..), Operator(..)) where

-- | Arithmetic expressions.
data Expr a = Op Operator (Expr a) (Expr a) -- ^ Arithmetic Operation Node
            | Var String                    -- ^ Input Variable Node
            | Literal a                     -- ^ Literal Value Node
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
