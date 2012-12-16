#!/bin/bash

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
cd "$HERE"

NAME1="AllInOne      "
PROG1="../dist/build/AllInOne/AllInOne"
NAME2="run-evaluation"
PROG2="../run-evaluation.sh"
DEBUG=0
F97=0

function usage() {
   echo "$0 [-f] [-d]"
   echo
   echo "-f: use F97"
   echo "-d: debug"
}

while getopts fd OPT; do
    case "$OPT" in
        f)
            F97=1
            ;;
        d)
            DEBUG=1
            ;;
        [?])
            usage
            exit 1
            ;;
    esac
done

#<prog> <poly-file> <input> <expected>
function run_sample() {
    PROG="$1"
    PFILE="$2"
    EL="$3"
    EXP="$4"
    TMPOUT=$(mktemp /tmp/out.XXXXXX)
    RET=0

    if [ $DEBUG -ne 0 ]; then
        "$PROG" -n "$PFILE" "$EL" $POLY_EVAL_OPTS -q $PROGOPTS 2>&1 |\
             tee "$TMPOUT"
    else
        "$PROG" -n "$PFILE" "$EL" $POLY_EVAL_OPTS -q $PROGOPTS &> "$TMPOUT"
    fi

    if grep -q "DAVID DONE, final result = Just ${EXP}\$" "$TMPOUT";then
        RET=0
    else
        RET=1
    fi
    rm -- "$TMPOUT"
    return $RET
}

################################################################################
# T E S T S
################################################################################

# F2Pow256
function f256_poly_sum_a() {
    write_poly <<EOF
[1 0 1 1 0 0 0 1]
[1 0 1 0 1]
[]
[1]
[1 1]
EOF
    G_INPUT="[1 1 1 0 0 1 1 1]"
    G_EXPECTED="\[0 1 1 1 1 1 0 1 1 1 1 1 0 1 0 0 0 0 1 0 0 0 0 0 1 1 0 0 1 1\]"
}

function f256_poly_sum_b() {
    write_poly <<EOF
[1 0 1 1 0 0 0 1]
[1 0 1 0 1]
EOF
    G_INPUT="[1 1 1 0 0 1 1 1]"
    G_EXPECTED="\[0 1 1 0 0 0 0 1 1 0 1 1\]"
}

function f256_poly_sum_c() {
    write_poly <<EOF
[]
[]
[]
[1]
[1 1]
EOF
    G_INPUT="[1 1 1 0 0 1 1 1]"
    G_EXPECTED="\[0 0 0 1 1 1 0 0 0 1 0 0 0 1 0 0 0 0 1 0 0 0 0 0 1 1 0 0 1 1\]"
}

function f256_poly_sum_d() {
    write_poly <<EOF
[0 1 1 0 0 0 0 1 1 0 1 1]
[1]
EOF
    G_INPUT="[0 0 0 1 1 1 0 0 0 1 0 0 0 1 0 0 0 0 1 0 0 0 0 0 1 1 0 0 1 1]"
    G_EXPECTED="\[0 1 1 1 1 1 0 1 1 1 1 1 0 1 0 0 0 0 1 0 0 0 0 0 1 1 0 0 1 1\]"
}

# F97
function f97_test1() {
    write_poly <<EOF
1
2
3
EOF
    G_INPUT=4
    let G_EXPECTED=(1+2*4+3*4**2)%97
}

function f97_test2() {
    write_poly <<EOF
13
27
12
0
0
14
EOF
    G_INPUT=23
    let G_EXPECTED=(13+27*23+12*23**2+14*23**5)%97
}
################################################################################
F256_TESTS="f256_poly_sum_a f256_poly_sum_b f256_poly_sum_c f256_poly_sum_d"
F97_TESTS="f97_test1 f97_test2"

function write_poly() {
    cat > "$G_POLY"
}

FAILED=0
WRONG=0
if [ $F97 -ne 0 ]; then
    if ! grep -qE '^type Element.*\<F97\>' ../programs/StaticConf*.hs; then
        echo "WARNING: Source seems not to be compiled for F97"
        WRONG=1
    fi
    TESTS="$F97_TESTS"
else
    if ! grep -qE '^type Element.*\<F2Pow256\>' ../programs/StaticConf*.hs; then
        echo "WARNING: Source seems not to be compiled for F97"
        WRONG=1
    fi
    TESTS="$F256_TESTS"
fi

for t in $TESTS; do
    echo "- Running Test $t"
    for p in 1 2; do
        PROGVAR="PROG$p"
        NAMEVAR="NAME$p"
        PROG=${!PROGVAR}
        NAME=${!NAMEVAR}
        if [ $DEBUG -ne 0 ]; then
            echo "${NAME}: $PROG"
        fi

        F=$(mktemp /tmp/poly.XXXXXX)
        G_POLY="$F"
        $t
        if [ $DEBUG -ne 0 ]; then
            echo "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv"
            echo -n "RUNNING: "
            echo run_sample "$PROG" "$G_POLY" "$G_INPUT" "$G_EXPECTED"
            echo "-------------------------------------------------------------"
        fi
        run_sample "$PROG" "$G_POLY" "$G_INPUT" "$G_EXPECTED"
        SUC=$?
        if [ $DEBUG -ne 0 -a $SUC -ne 0 ]; then
            echo "PRESERVED POLY FILE: $F"
        else
            rm "$F"
        fi
        if [ $DEBUG -ne 0 ]; then
            echo "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
        fi

        if [ $SUC -eq 0 ]; then
            echo -e "    $NAME: SUCCESS"
        else
            echo -e "    $NAME: FAILED"
            let FAILED=$FAILED+1
        fi
    done
done

if [ $FAILED -ne 0 -a $WRONG -eq 0 ]; then
    echo '#######################################'
    echo "# WARNING: $FAILED FAILURES"
    echo '#######################################'
fi
