#!/bin/bash

set -e

function error() {
    echo "AAH ERROR"
}

function kill_child_processes() {
    trap - ERR
    echo "Ctrl+C caught, Killing child processes"
    KILLED=""
    while read cpid; do
        kill "$cpid" 2> /dev/null || true
        KILLED="$KILLED $cpid"
    done < <(jobs -p)
    echo "Killed $KILLED"
}

function current_time_millis() {
    echo $(($(date +%s%N)/1000000));
}

#trap error ERR
trap kill_child_processes SIGINT

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null && pwd)
cd "$HERE"

DIRECT=0
if [ "$1" = "-n" ]; then
    DIRECT=1
    shift
fi
if [ $# -lt 2 ]; then
    echo "Usage: $0 POLY-FILE INPUT-ELEMENT"
    exit 1
fi
POLYFILE="$1"
INPUTELEM="$2"
shift
shift
if [ ! -r "$POLYFILE" ]; then
    echo "ERROR: Cannot read polynomial file '$POLYFILE'"
    exit 1
fi

echo "Polynomial in file '$POLYFILE', input='$INPUTELEM'"
function unbuffer() {
    if [ -x /usr/bin/stdbuf ]; then
        stdbuf -i0 -o0 -e0 "$@"
    else
        "$@"
    fi
}

# NAME
function run_component() {
    NAME="$1"
    shift
    echo "$NAME: Starting"
    TIMES=$(current_time_millis)
    if [ $DIRECT -eq 1 ]; then
        unbuffer dist/build/$NAME/$NAME "$@"
    else
        while read line; do
            echo "$NAME: $line"
        done < <(unbuffer dist/build/$NAME/$NAME "$@")
    fi
    TIMEE=$(current_time_millis)
    let TIME=$TIMEE-$TIMES
    echo "$NAME: Exited (running ${TIME}ms)"
}

function run_component_bg() {
    run_component "$@" &
}

set +e
killall David Goliath Token &> /dev/null
set -e

run_component_bg Goliath "$POLYFILE" "$@"
GPID=$!
cat "$POLYFILE" | while read line; do sleep 0.001; done
run_component_bg Token "$@"
TPID=$!
sleep 0.5
run_component_bg David "$INPUTELEM" "$@"
DPID=$!

echo $DPID
wait $DPID
echo $DPID $TPID $GPID

set +e
jobs -p | while read line; do
    echo "Killing $line"
    kill -INT $line &> /dev/null
done
killall -INT David Goliath Token &> /dev/null
