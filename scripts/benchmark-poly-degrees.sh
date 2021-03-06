#!/bin/bash

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
cd "$HERE"

PROG="../dist/build/AllInOne/AllInOne"
PROGOPTS=""
WARMUPS=1
RUNS=3
STEP=500
MIN=500
MAX=6000
POLY_EVAL_OPTS="-h"
OUT="-"
DEBUG=0
F97=0

function usage() {
   echo "$0 [-o OUT] [-w WARMUP-RUNS] [-r RUNS] [-s STEP] [-m MIN] [-M MAX]"\
" [-n] [-f] [-b BINARY] [-o OPTS]"
   echo
   echo "-o: output file (default: $OUT)"
   echo "-n: don't use Horner's rule (aka monomial form, discouraged)"
   echo "-w: warmup runs (default: $WARMUPS)"
   echo "-m: minimal degree (default: $MIN)"
   echo "-M: maximal degree (default: $MAX)"
   echo "-r: counted runs (default: $RUNS)"
   echo "-s: degree step (default: $STEP)"
   echo "-f: use F97"
   echo "-b: binary (default: $PROG)"
   echo "-O: other options to binary (default: $PROGOPTS)"
}

while getopts w:r:s:m:M:no:dfb:O: OPT; do
    case "$OPT" in
        w)
            WARMUPS="$OPTARG"
            ;;
        r)
            RUNS="$OPTARG"
            ;;
        s)
            STEP="$OPTARG"
            ;;
        m)
            MIN="$OPTARG"
            ;;
        M)
            MAX="$OPTARG"
            ;;
        n)
            POLY_EVAL_OPTS="-m"
            ;;
        o)
            OUT="$OPTARG"
            ;;
        d)
            DEBUG=1
            ;;
        f)
            F97=1
            ;;
        b)
            PROG="$OPTARG"
            ;;
        O)
            PROGOPTS="$OPTARG"
            ;;
        [?])
            usage
            exit 1
            ;;
    esac
done

function get_random_element() {
    if [ $F97 -ne 0 ]; then
        echo 17
    else
        echo '[1 1 0 0 1 0 1]'
    fi
}

function random_poly_file() {
    PFILE=$(mktemp /tmp/poly.XXXXXX)
    for (( i=0; i<$1; i=$i+1 )); do
        get_random_element >> "$PFILE"
    done
    echo "$PFILE"
}

function write_out() {
    if [ "$OUT" = "-" ]; then
        cat
    else
        tee -a "$OUT" > /dev/null
    fi
}

#<poly-degree> <poly-file> <random-element>
function run_sample() {
    REAL=1
    if [ "$1" = "-n" ]; then
        REAL=0
        shift
    fi
    PDEGREE="$1"
    PFILE="$2"
    EL="$3"
    TMPOUT=$(mktemp /tmp/out.XXXXXX)
    RET=0

    if [ $(wc -l "$PFILE" | grep -Eo '[0-9]+' | head -1) -ne $PDEGREE ]; then
        echo "ERROR: length of poly file does not equal degree"
        exit 2
    fi

    if [ $DEBUG -ne 0 ]; then
        "$PROG" "$PFILE" "$EL" $POLY_EVAL_OPTS -q $PROGOPTS 2>&1 | tee "$TMPOUT"
    else
        "$PROG" "$PFILE" "$EL" $POLY_EVAL_OPTS -q $PROGOPTS &> "$TMPOUT"
    fi
    #echo "David: DAVID DONE, final result = Just" > "$TMPOUT"
    #echo "David: Exited (running 718ms)" >> "$TMPOUT"

    T=$(grep -E 'David: Exited \(running [0-9]+ms\)' "$TMPOUT" |\
        grep -Eo '[0-9]+')
    if grep -q 'David: DAVID DONE, final result = Just' "$TMPOUT"; then
        if [ $REAL -ne 0 ]; then
            echo -e "$PDEGREE\t$T" | write_out
        else
            echo >&2 'WARMUP RUN SUCCESSFULLY'
        fi
        RET=0
    else
        echo >&2 "FAIL, degree: $PDEGREE"
        RET=1
    fi
    rm -- "$TMPOUT"
    return $RET
}

if [ "$OUT" != "-" ]; then
    echo -n > "$OUT"
fi
echo -e "Polynomial-Degree\tTime" | write_out
for (( i=$MIN; i<=$MAX; i=$i+${STEP} )); do
    for r in $(seq "$WARMUPS"); do
        PFILE=$(random_poly_file $i)
        run_sample -n $i "$PFILE" "$(get_random_element)"
        rm -- "$PFILE"
    done
    for r in $(seq "$RUNS"); do
        PFILE=$(random_poly_file $i)
        for f in $(seq 3); do
            run_sample $i "$PFILE" "$(get_random_element)"
            if [ $? -eq 0 ]; then
                break
            else
                echo >&2 "FAILURE no. $f"
            fi
        done
        rm -- "$PFILE"
    done
done
