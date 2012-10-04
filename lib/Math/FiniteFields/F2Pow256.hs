{-# LANGUAGE BangPatterns #-}
module Math.FiniteFields.F2Pow256 ( F2Pow256, f2Pow256FromString
                                  , f2Pow256FromUtf8ByteString
                                  , f2Pow256ToUtf8ByteString
                                  , f2Pow256ToBytes
                                  , f2Pow256FromBytes
                                  ) where

import Data.ByteString (ByteString)
import Data.Helpers (integralBytes)
import Data.FieldTypes
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, castPtr)
import Control.Monad.CryptoRandom (CRandom(..))
import Crypto.Random (CryptoRandomGen(..))

import qualified Math.FiniteFields.Foreign.FFInterface as FFI

newtype F2Pow256 = F2Pow256 { unF2Pow256 :: FFI.OpaqueElement }

binaryOp :: (FFI.OpaqueElement -> FFI.OpaqueElement -> FFI.OpaqueElement)
         -> F2Pow256 -> F2Pow256 -> F2Pow256
binaryOp op (F2Pow256 !l) (F2Pow256 !r) =
   let result = l `op` r
    in result `seq` F2Pow256 result

instance Field F2Pow256 where
    invert = F2Pow256 . FFI.ffInvertElement . unF2Pow256
    one = fromIntegerF2Pow256 1
    zero = fromIntegerF2Pow256 0

instance Show F2Pow256 where
    show = FFI.ffElementToString . unF2Pow256

instance Read F2Pow256 where
    readsPrec _ value = [((F2Pow256 . FFI.ffElementFromString) value, "")]

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

instance CRandom F2Pow256 where
    crandom g =
        case genBytes 32 g of
          Right (bs, g') -> Right (f2Pow256FromBytes bs, g')
          Left err -> Left err

fromIntegerF2Pow256 :: Integer -> F2Pow256
fromIntegerF2Pow256  = F2Pow256 . FFI.ffElementFromBytes . integralBytes . abs

f2Pow256FromString :: String -> F2Pow256
f2Pow256FromString = F2Pow256 . FFI.ffElementFromString

f2Pow256FromUtf8ByteString :: ByteString -> F2Pow256
f2Pow256FromUtf8ByteString = F2Pow256 . FFI.ffElementFromUtf8ByteString

f2Pow256ToUtf8ByteString :: F2Pow256 -> ByteString
f2Pow256ToUtf8ByteString = FFI.ffElementToUtf8ByteString . unF2Pow256

f2Pow256ToBytes :: F2Pow256 -> ByteString
f2Pow256ToBytes = FFI.ffElementToBytes . unF2Pow256

f2Pow256FromBytes :: ByteString -> F2Pow256
f2Pow256FromBytes = F2Pow256 . FFI.ffElementFromBytes

instance Eq F2Pow256 where
    (==) (F2Pow256 l) (F2Pow256 r) = FFI.ffEquals l r
