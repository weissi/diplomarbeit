#!/bin/bash

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null && pwd)

cd "$HERE"

for f in protos/*.proto; do
    hprotoc -I protos -d gen-src -p Data.ProtoBufs "$(basename $f)"
done
