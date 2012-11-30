-- | Simply to debug the implementation.
--
-- Don't use this module in production code.
module Math.FiniteFields.DebugField (DebugField) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8
import Control.Monad.CryptoRandom (CRandom(..))
import Data.FieldTypes (Field(..))
import Data.RAE.Conduit (ByteSerializable(..))

newtype DebugField = DebugField { unDF :: String }

instance ByteSerializable DebugField where
    serializeBytes = BSL.fromStrict . BS8.pack . unDF
    parseBytes = DebugField . BS8.unpack . BSL.toStrict
instance Field DebugField where
    invert (DebugField s) = DebugField $ "("++s++")^-1"
    one = DebugField "1"
    zero = DebugField "0"
instance Num DebugField where
    (+) (DebugField l) (DebugField r) = DebugField $ "(" ++ l ++ "+" ++ r ++ ")"
    (-) (DebugField l) (DebugField r) = DebugField $ "(" ++ l ++ "-" ++ r ++ ")"
    (*) (DebugField l) (DebugField r) = DebugField $ "(" ++ l ++ "*" ++ r ++ ")"
    negate (DebugField e) = DebugField $ "(-"++e++")"
    signum (DebugField e) = DebugField $ "signum("++e++")"
    abs (DebugField e) = DebugField $ "|"++e++"|"
    fromInteger i = DebugField $ show i
instance CRandom DebugField where
    crandom g =
        case crandom g of
           Right (i, g') ->
               Right (DebugField $ "R"++ (show ((i::Int) `mod` 9999)), g')
           Left e -> Left e

instance Eq DebugField where
    (==) (DebugField l) (DebugField r) = l == r
instance Read DebugField where
    readsPrec n s = (map (\(a,b) -> (DebugField a, b)) . (readsPrec n)) s
instance Show DebugField where
    show (DebugField e) = show e
