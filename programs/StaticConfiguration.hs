module StaticConfiguration where

import qualified Data.Conduit.Network as CN

import Control.Monad.Trans.Resource (ResourceT)
import Data.ExpressionTypes
import Data.FieldTypes
import Data.RAE.Conduit (ByteSerializable(..))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL

--import Math.FiniteFields.F2Pow256
--type Element = F2Pow256

import Math.Algebra.Field.Base (F97, Fp)
import Math.Common.IntegerAsType (IntegerAsType)
import Control.Monad.CryptoRandom (CRandom(..))
type Element = F97
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

_X_ :: Expr Element
_X_ = Var "x"

type RMonad = ResourceT IO

-- Goliath --> Token
_CLIENT_CONF_GOLIATH_TO_TOKEN_ :: CN.ClientSettings RMonad
_CLIENT_CONF_GOLIATH_TO_TOKEN_ = CN.clientSettings 23120 (BS8.pack "127.0.0.1")

_SRV_CONF_TOKEN_FROM_GOLIATH_ :: CN.ServerSettings RMonad
_SRV_CONF_TOKEN_FROM_GOLIATH_ = CN.serverSettings 23120 CN.HostAny

-- Goliath --> David
_CLIENT_CONF_GOLIATH_TO_DAVID_ :: CN.ClientSettings RMonad
_CLIENT_CONF_GOLIATH_TO_DAVID_ = CN.clientSettings 23102 (BS8.pack "127.0.0.1")

_SRV_CONF_DAVID_FROM_GOLIATH_ :: CN.ServerSettings RMonad
_SRV_CONF_DAVID_FROM_GOLIATH_ = CN.serverSettings 23102 CN.HostAny

-- David --> Goliath
_CLIENT_CONF_DAVID_TO_GOLIATH_ :: CN.ClientSettings RMonad
_CLIENT_CONF_DAVID_TO_GOLIATH_ = CN.clientSettings 23201 (BS8.pack "127.0.0.1")

_SRV_CONF_GOLIATH_FROM_DAVID_ :: CN.ServerSettings RMonad
_SRV_CONF_GOLIATH_FROM_DAVID_ = CN.serverSettings 23201 CN.HostAny

-- David --> Token
_CLIENT_CONF_DAVID_TO_TOKEN_ :: CN.ClientSettings RMonad
_CLIENT_CONF_DAVID_TO_TOKEN_ = CN.clientSettings 23021 (BS8.pack "127.0.0.1")

_SRV_CONF_TOKEN_FROM_DAVID_ :: CN.ServerSettings RMonad
_SRV_CONF_TOKEN_FROM_DAVID_ = CN.serverSettings 23021 CN.HostAny
