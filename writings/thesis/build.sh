#!/bin/bash

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
cd "$HERE"

if [ "$1" = "-c" ]; then
    echo "Building in continuous mode"
    iwatch -e modify -t '\.tex$' -c ./build.sh .
    exit 0
fi

STEP="main program"
function err() {
    echo "UNEXPECTED ERROR (in $STEP)"
    exit 1
}

trap err ERR

TEXOPTS="-output-directory build -halt-on-error"

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
echo == pdfLaTeX 1 =============================================================
try_pdflatex $TEXOPTS thesis
STEP="bibtex"
echo == BibTeX =================================================================
bibtex build/thesis
STEP="pdflatex run 2"
echo == pdfLaTeX 2 =============================================================
try_pdflatex $TEXOPTS thesis
STEP="pdflatex run 3"
echo == pdfLaTeX 3 =============================================================
pdflatex $TEXOPTS -halt-on-error thesis 2>&1 | tee /tmp/pdflatex.out
STEP="main program"
cp build/thesis.pdf .

echo BUILD SUCCESSFUL

case "$(uname -s)" in
    Darwin)
        open thesis.pdf
        ;;
    Linux)
        if pgrep -f 'evince (.+\/)?thesis.pdf' > /dev/null; then
            export PATH="$PATH:/home/weissi/local/bin/"
            evince thesis.pdf
            pgrep grive && echo "NOTICE: grive already uploading" || \
                { ( cd ~/gdrive/ && date && grive )&>>/tmp/grive.log & }
        fi
        ;;
    *)
        echo >&2 "ERROR: could not open PDF under OS '$(uname -s)'"
        ;;
esac
