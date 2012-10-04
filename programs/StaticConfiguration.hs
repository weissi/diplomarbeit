module StaticConfiguration where

import qualified Data.Conduit.Network as CN

import Data.ExpressionTypes
import Math.FiniteFields.F2Pow256

type Element = F2Pow256

_X_ :: Expr Element
_X_ = Var "x"

_TEST_EXPR_ :: Expr Element
_TEST_EXPR_ = _X_ ^ (10000::Integer) {-1 + _X_ * _X_ * _X_ * _X_ * _X_ * _X_ *
              (foldl (*) 1 $ replicate 1000 _X_)-}

-- Goliath --> Token
_CLIENT_CONF_GOLIATH_TO_TOKEN_ :: CN.ClientSettings
_CLIENT_CONF_GOLIATH_TO_TOKEN_ = CN.ClientSettings 23120 "localhost"

_SRV_CONF_TOKEN_FROM_GOLIATH_ :: CN.ServerSettings
_SRV_CONF_TOKEN_FROM_GOLIATH_ = CN.ServerSettings 23120 CN.HostAny

-- Goliath --> David
_CLIENT_CONF_GOLIATH_TO_DAVID_ :: CN.ClientSettings
_CLIENT_CONF_GOLIATH_TO_DAVID_ = CN.ClientSettings 23102 "localhost"

_SRV_CONF_DAVID_FROM_GOLIATH_ :: CN.ServerSettings
_SRV_CONF_DAVID_FROM_GOLIATH_ = CN.ServerSettings 23102 CN.HostAny

-- David --> Goliath
_CLIENT_CONF_DAVID_TO_GOLIATH_ :: CN.ClientSettings
_CLIENT_CONF_DAVID_TO_GOLIATH_ = CN.ClientSettings 23201 "localhost"

_SRV_CONF_GOLIATH_FROM_DAVID_ :: CN.ServerSettings
_SRV_CONF_GOLIATH_FROM_DAVID_ = CN.ServerSettings 23201 CN.HostAny

-- David --> Token
_CLIENT_CONF_DAVID_TO_TOKEN_ :: CN.ClientSettings
_CLIENT_CONF_DAVID_TO_TOKEN_ = CN.ClientSettings 23021 "localhost"

_SRV_CONF_TOKEN_FROM_DAVID_ :: CN.ServerSettings
_SRV_CONF_TOKEN_FROM_DAVID_ = CN.ServerSettings 23021 CN.HostAny
