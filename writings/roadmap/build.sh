#!/bin/bash

STEP="main program"
function err() {
    echo "UNEXPECTED ERROR (in $STEP)"
    exit 1
}

trap err ERR

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
TEXOPTS="-output-directory build -halt-on-error"
cd "$HERE"

function try_pdflatex() {
    set +e
    TMP=$(mktemp /tmp/studi-build-XXXXXX)
    pdflatex "$@" &> "$TMP"
    if [ $? -eq 0 ]; then
        rm -- "$TMP"
        set -e
        return 0
    else
        cat -- "$TMP"
        rm -- "$TMP"
        set -e
        return 1
    fi
}

./clean.sh

set -e
STEP="pdflatex run 1"
try_pdflatex $TEXOPTS roadmap
STEP="bibtex"
bibtex build/roadmap
STEP="pdflatex run 2"
try_pdflatex $TEXOPTS roadmap
STEP="pdflatex run 3"
pdflatex $TEXOPTS -halt-on-error roadmap 2>&1 | tee /tmp/pdflatex.out
STEP="main program"
cp build/roadmap.pdf .

echo BUILD SUCCESSFUL

case "$(uname -s)" in
    Darwin)
        open roadmap.pdf
        ;;
    Linux)
        if pgrep -f 'evince (.+\/)?thesis.pdf' > /dev/null; then
            evince roadmap.pdf
        fi
        ;;
    *)
        echo >&2 "ERROR: could not open PDF under OS '$(uname -s)'"
        ;;
esac
