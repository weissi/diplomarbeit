#!/bin/bash

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
cd "$HERE"

set -e

F97=0
NORMAL=0

function usage() {
   echo "$0 -f|-n"
   echo
   echo "-f: switch to F97"
   echo "-n: switch to 'normal' F2Pow256"
}

while getopts fn OPT; do
    case "$OPT" in
        f)
            F97=1
            ;;
        n)
            NORMAL=1
            ;;
        [?])
            usage
            exit 1
            ;;
    esac
done

function echo_switch_f97() {
    cat <<EOF
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

module StaticConfiguration where

import qualified Data.Conduit.Network as CN

import Data.ExpressionTypes
import Data.String
import qualified Data.ByteString.Char8 as BS8

--import Math.FiniteFields.F2Pow256
--type Element = F2Pow256

import Math.FiniteFields.F97
type Element = F97

--import Math.FiniteFields.DebugField
--type Element = DebugField


_X_ :: Num a => Expr a
_X_ = Var "x"

-- Goliath --> Token
_CLIENT_CONF_GOLIATH_TO_TOKEN_ :: CN.ClientSettings
_CLIENT_CONF_GOLIATH_TO_TOKEN_ = CN.clientSettings 23120 (BS8.pack "127.0.0.1")

_SRV_CONF_TOKEN_FROM_GOLIATH_ :: CN.ServerSettings
_SRV_CONF_TOKEN_FROM_GOLIATH_ = CN.serverSettings 23120 (fromString "*")

-- Goliath --> David
_CLIENT_CONF_GOLIATH_TO_DAVID_ :: CN.ClientSettings
_CLIENT_CONF_GOLIATH_TO_DAVID_ = CN.clientSettings 23102 (BS8.pack "127.0.0.1")

_SRV_CONF_DAVID_FROM_GOLIATH_ :: CN.ServerSettings
_SRV_CONF_DAVID_FROM_GOLIATH_ = CN.serverSettings 23102 (fromString "*")

-- David --> Goliath
_CLIENT_CONF_DAVID_TO_GOLIATH_ :: CN.ClientSettings
_CLIENT_CONF_DAVID_TO_GOLIATH_ = CN.clientSettings 23201 (BS8.pack "127.0.0.1")

_SRV_CONF_GOLIATH_FROM_DAVID_ :: CN.ServerSettings
_SRV_CONF_GOLIATH_FROM_DAVID_ = CN.serverSettings 23201 (fromString "*")

-- David --> Token
_CLIENT_CONF_DAVID_TO_TOKEN_ :: CN.ClientSettings
_CLIENT_CONF_DAVID_TO_TOKEN_ = CN.clientSettings 23021 (BS8.pack "127.0.0.1")

_SRV_CONF_TOKEN_FROM_DAVID_ :: CN.ServerSettings
_SRV_CONF_TOKEN_FROM_DAVID_ = CN.serverSettings 23021 (fromString "*")
EOF
}

function echo_switch_f2pow256() {
    cat <<EOF
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

module StaticConfiguration where

import qualified Data.Conduit.Network as CN

import Data.ExpressionTypes
import Data.String
import qualified Data.ByteString.Char8 as BS8

import Math.FiniteFields.F2Pow256
type Element = F2Pow256

--import Math.FiniteFields.F97
--type Element = F97

--import Math.FiniteFields.DebugField
--type Element = DebugField


_X_ :: Num a => Expr a
_X_ = Var "x"

-- Goliath --> Token
_CLIENT_CONF_GOLIATH_TO_TOKEN_ :: CN.ClientSettings
_CLIENT_CONF_GOLIATH_TO_TOKEN_ = CN.clientSettings 23120 (BS8.pack "127.0.0.1")

_SRV_CONF_TOKEN_FROM_GOLIATH_ :: CN.ServerSettings
_SRV_CONF_TOKEN_FROM_GOLIATH_ = CN.serverSettings 23120 (fromString "*")

-- Goliath --> David
_CLIENT_CONF_GOLIATH_TO_DAVID_ :: CN.ClientSettings
_CLIENT_CONF_GOLIATH_TO_DAVID_ = CN.clientSettings 23102 (BS8.pack "127.0.0.1")

_SRV_CONF_DAVID_FROM_GOLIATH_ :: CN.ServerSettings
_SRV_CONF_DAVID_FROM_GOLIATH_ = CN.serverSettings 23102 (fromString "*")

-- David --> Goliath
_CLIENT_CONF_DAVID_TO_GOLIATH_ :: CN.ClientSettings
_CLIENT_CONF_DAVID_TO_GOLIATH_ = CN.clientSettings 23201 (BS8.pack "127.0.0.1")

_SRV_CONF_GOLIATH_FROM_DAVID_ :: CN.ServerSettings
_SRV_CONF_GOLIATH_FROM_DAVID_ = CN.serverSettings 23201 (fromString "*")

-- David --> Token
_CLIENT_CONF_DAVID_TO_TOKEN_ :: CN.ClientSettings
_CLIENT_CONF_DAVID_TO_TOKEN_ = CN.clientSettings 23021 (BS8.pack "127.0.0.1")

_SRV_CONF_TOKEN_FROM_DAVID_ :: CN.ServerSettings
_SRV_CONF_TOKEN_FROM_DAVID_ = CN.serverSettings 23021 (fromString "*")
EOF
}

CONF="../programs/StaticConfiguration.hs"
if [ ! -f "$CONF" ]; then
    echo "ERROR: $CONF not found"
    exit 1
fi

if [ $F97 -ne 0 ]; then
    echo "NOTICE: Switching to F97"
    echo_switch_f97 > "$CONF"
    echo "SUCCESS, recompiling"
    cd ..
    cabal build
    exit 0
fi

if [ $NORMAL -ne 0 ]; then
    echo "NOTICE: Switching to F2Pow256"
    echo_switch_f2pow256 > "$CONF"
    echo "SUCCESS, recompiling"
    cd ..
    cabal build
    exit 0
fi

echo "WARNING: Nothing done, please pass -f or -n"
