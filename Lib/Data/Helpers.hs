module Data.Helpers (integralBytes) where

import Blaze.ByteString.Builder (toByteString)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Data.ByteString
import Data.Char (chr)
import Data.Monoid (mempty, mappend)

integralBytes :: (Integral a, Show a) => a -> ByteString
integralBytes n0
  | n0 <  0   = error ("Numeric.showIntAtBase: applied to negative number " ++ show n0)
  | otherwise = toByteString $ showIt (quotRem n0 256) mempty
   where
    showIt (n,d) r = seq c $ -- stricter than necessary
      case n of
        0 -> r'
        _ -> showIt (quotRem n 256) r'
     where
      c  = fromChar $ chr (fromIntegral d)
      r' = c `mappend` r
