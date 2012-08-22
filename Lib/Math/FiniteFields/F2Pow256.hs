{-# LANGUAGE BangPatterns #-}

module Math.FiniteFields.F2Pow256 (F2Pow256) where

import Data.FieldTypes

import qualified Math.FiniteFields.Foreign.FFInterface as FFI

type OpaqueElement = FFI.OpaqueElement

newtype F2Pow256 = F2Pow256 OpaqueElement

unF2Pow256 :: F2Pow256 -> OpaqueElement
unF2Pow256 (F2Pow256 !e) = e

binaryOp :: (OpaqueElement -> OpaqueElement -> OpaqueElement)
         -> (F2Pow256 -> F2Pow256 -> F2Pow256)
binaryOp op = \(F2Pow256 l) (F2Pow256 r) -> F2Pow256 $ l `op` r

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
    fromInteger _ = F2Pow256 FFI.ffOneElement

instance Eq F2Pow256 where
    (==) (F2Pow256 l) (F2Pow256 r) = FFI.ffEquals l r
