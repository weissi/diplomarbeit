module Math.FiniteFields.F2Pow256 (F2Pow256, f2Pow256FromString) where

import Data.Helpers (integralBytes)
import Data.FieldTypes

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

fromIntegerF2Pow256 :: Integer -> F2Pow256
fromIntegerF2Pow256 = F2Pow256 . FFI.ffElementFromBytes . integralBytes . abs

f2Pow256FromString :: String -> F2Pow256
f2Pow256FromString = F2Pow256 . FFI.ffElementFromString

instance Eq F2Pow256 where
    (==) (F2Pow256 l) (F2Pow256 r) = FFI.ffEquals l r
