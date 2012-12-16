#!/bin/bash

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
cd "$HERE/.."

HACKAGEURL='http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html'

cabal haddock --html-location="$HACKAGEURL" --hyperlink-source
