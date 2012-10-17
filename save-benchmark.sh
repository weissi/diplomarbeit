#!/bin/bash

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null && pwd)

set -e

cd "$HERE"
FILE="$(date +%Y-%m-%d_%H-%M-%S).text"
(
function unbuffer() {
    if [ -x /usr/bin/stdbuf ]; then
        stdbuf -i0 -o0 -e0 "$@"
    else
        "$@"
    fi
}

echo "Versions"
echo "--------"
uname -a
ghc --version
echo
echo "Last patch"
echo "----------"
git log HEAD^..HEAD
echo
echo "Modifications"
echo "-------------"
git status
echo
echo "Benchmark"
echo "---------"
unbuffer cabal bench
) | tee benchmark-results/$FILE

echo OK
