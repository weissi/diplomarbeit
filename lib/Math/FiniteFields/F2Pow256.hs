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
import Control.Monad.CryptoRandom (CRandom(..))
import Crypto.Random (CryptoRandomGen(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (choose)
import Text.Regex (Regex, mkRegex, matchRegex)

import qualified Math.FiniteFields.Foreign.FFInterface as FFI

newtype F2Pow256 = F2Pow256 { unF2Pow256 :: FFI.OpaqueElement }

binaryOp :: (FFI.OpaqueElement -> FFI.OpaqueElement -> FFI.OpaqueElement)
         -> F2Pow256 -> F2Pow256 -> F2Pow256
binaryOp op (F2Pow256 !l) (F2Pow256 !r) =
   let result = l `op` r
    in result `seq` F2Pow256 result

_READ_REGEX_ :: Regex
_READ_REGEX_ = mkRegex "^(\\[([01]( [01]){0,255})?\\])"

instance Field F2Pow256 where
    invert = F2Pow256 . FFI.ffInvertElement . unF2Pow256
    one = fromIntegerF2Pow256 1
    zero = fromIntegerF2Pow256 0

instance Show F2Pow256 where
    show = FFI.ffElementToString . unF2Pow256

instance Read F2Pow256 where
    readsPrec _ str =
        case matchRegex _READ_REGEX_ str of
          Nothing -> []
          Just xs ->
             case xs of
              [] -> []
              (value:_) ->
                 let rest = drop (length value) str
                  in [((F2Pow256 . FFI.ffElementFromString) value, rest)]

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
    (==) (F2Pow256 !l) (F2Pow256 !r) = let rs = FFI.ffEquals l r in rs `seq` rs

instance Arbitrary F2Pow256 where
    arbitrary = fmap fromInteger $ choose (0, (2::Integer)^(256::Integer)-1)
