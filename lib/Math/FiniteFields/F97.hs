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

-- | Adds some instances for @F97@, this is dangerous!
-- Don't use this module in production code.
module Math.FiniteFields.F97 (F97) where

import Math.Algebra.Field.Base (F97, Fp)
import Math.Common.IntegerAsType (IntegerAsType)
import Control.Monad.CryptoRandom (CRandom(..))
import Test.QuickCheck.Arbitrary ( Arbitrary(..), arbitrary
                                 , arbitrarySizedIntegral
                                 )
import System.Random (Random(..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8

import Data.FieldTypes (Field(..))
import Data.RAE.Conduit (ByteSerializable(..))

instance IntegerAsType n => Field (Fp n) where
    invert n =
        case n of
          0 -> error "0 is not invertible"
          n' -> 1 / n'
    one = 1
    zero = 0

instance IntegerAsType n => Read (Fp n) where
    readsPrec n str = map (\(f,s) -> (fromInteger f, s)) (readsPrec n str)

instance IntegerAsType n => ByteSerializable (Fp n) where
    serializeBytes = BSL.fromStrict . BS8.pack . show
    parseBytes = read . BS8.unpack . BSL.toStrict

instance IntegerAsType n => CRandom (Fp n) where
    crandom g =
        case crandom g of
          Left err -> Left err
          Right (a, g') -> Right (fromIntegral (a :: Int), g')

instance IntegerAsType n => Arbitrary (Fp n) where
    arbitrary = fmap fromIntegral arbitrarySizedIntegral

instance IntegerAsType n => Random (Fp n) where
    random g =
        let (rint, g') = random g
            rint' = rint :: Integer
            in (fromIntegral rint', g')
    randomR (lo, hi) g =
        let loint = (read . show) lo :: Integer
            hiint = (read . show) hi :: Integer
            (rint, g') = randomR (loint, hiint) g
            rint' = rint :: Integer
            rfp = fromIntegral rint'
            in (rfp, g')
