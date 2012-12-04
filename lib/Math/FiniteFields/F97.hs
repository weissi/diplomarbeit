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
    arbitrary = arbitrarySizedIntegral

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
