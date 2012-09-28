module Data.FieldTypes (Field(..)) where

class (Eq a, Num a) => Field a where
    invert :: a -> a
    zero :: a
    one :: a
