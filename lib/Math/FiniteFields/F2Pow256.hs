module Math.FiniteFields.F2Pow256 (F2Pow256, f2Pow256FromString) where

import Data.Helpers (integralBytes)
import Data.FieldTypes
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, castPtr)

import qualified Math.FiniteFields.Foreign.FFInterface as FFI

newtype F2Pow256 = F2Pow256 { unF2Pow256 :: FFI.OpaqueElement }

binaryOp :: (FFI.OpaqueElement -> FFI.OpaqueElement -> FFI.OpaqueElement)
         -> F2Pow256 -> F2Pow256 -> F2Pow256
binaryOp op (F2Pow256 l) (F2Pow256 r) = F2Pow256 $ l `op` r

instance FieldElement F2Pow256 where
    invert = F2Pow256 . FFI.ffInvertElement . unF2Pow256

instance Show F2Pow256 where
    show = FFI.ffElementToString . unF2Pow256

instance Num F2Pow256 where
    (+) = binaryOp FFI.ffAddElements
    (*) = binaryOp FFI.ffMulElements
    (-) = binaryOp FFI.ffSubElements
    signum = error "F2Pow256.signum not implemented"
    abs = error "F2Pow256.abs not implemented"
    fromInteger = fromIntegerF2Pow256

instance Fractional F2Pow256 where
    (/) = binaryOp FFI.ffDivElements
    fromRational = error "F2Pow256: fromRational undefined"

instance Storable F2Pow256 where
    sizeOf = sizeOf . unF2Pow256
    alignment = sizeOf . unF2Pow256
    peek p = fmap F2Pow256 (peek (castPtr p :: Ptr FFI.OpaqueElement))
    poke p a = poke (castPtr p) (unF2Pow256 a)

fromIntegerF2Pow256 :: Integer -> F2Pow256
fromIntegerF2Pow256 = F2Pow256 . FFI.ffElementFromBytes . integralBytes . abs

f2Pow256FromString :: String -> F2Pow256
f2Pow256FromString = F2Pow256 . FFI.ffElementFromString

instance Eq F2Pow256 where
    (==) (F2Pow256 l) (F2Pow256 r) = FFI.ffEquals l r
