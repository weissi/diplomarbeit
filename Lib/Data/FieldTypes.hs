module Data.FieldTypes (FieldElement(..)) where

class (Eq a, Num a) => FieldElement a where
    invert :: a -> a
