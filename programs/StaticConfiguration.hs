module StaticConfiguration where

import qualified Data.Conduit.Network as CN

import Control.Monad.Trans.Resource (ResourceT)
import Data.ExpressionTypes
import Math.FiniteFields.F2Pow256
import qualified Data.ByteString.Char8 as BS8

type Element = F2Pow256

_X_ :: Expr Element
_X_ = Var "x"

_TEST_EXPR_ :: Expr Element
_TEST_EXPR_ = _X_ ^ (10000::Integer) {-1 + _X_ * _X_ * _X_ * _X_ * _X_ * _X_ *
              (foldl (*) 1 $ replicate 1000 _X_)-}

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
