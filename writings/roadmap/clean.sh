#!/bin/bash

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
cd "$HERE"

find . \( -name '*.toc' -or -name '*.aux' \
    -or -name '*.bbl' -or -name '*.cfg' -or -name '*.log' -or -name '*.out' \
    -or -name '*.blg' \) -exec rm '{}' \;
