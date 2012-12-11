-- | Mathematical fields.
module Data.FieldTypes (Field(..)) where

-- | Basic mathematical field class.
class (Eq a, Num a) => Field a where
    -- | Invert a field element.
    invert :: a -> a
    -- | The field's /zero/ element.
    zero :: a
    -- | The field's /one/ element.
    one :: a
