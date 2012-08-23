{-# LANGUAGE BangPatterns #-}
module Data.Helpers (integralBytes) where

import Data.Bits (Bits, shiftR)
import Blaze.ByteString.Builder (Builder, toByteString)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Data.ByteString
import Data.Char (chr)
import Data.Monoid (mempty, mappend)

integralBytes :: (Integral a, Bits a, Show a) => a -> ByteString
integralBytes n0
  | n0 <  0   = error ("integralBytes: applied to negative number " ++ show n0)
  | otherwise = toByteString $ marshallIntByte n0 mempty
  where marshallIntByte :: (Show a, Bits a, Integral a)
                        => a -> Builder -> Builder
        marshallIntByte n acc =
           let !newChar = chr . fromIntegral $ n `mod` 256
               newBuilder = fromChar newChar
            in case n of
                 0 -> acc
                 _ -> marshallIntByte (n `shiftR` 8) (acc `mappend` newBuilder)
