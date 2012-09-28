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

#trap error ERR
trap kill_child_processes SIGINT

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null && pwd)
cd "$HERE"

DIRECT=0
if [ "$1" = "-n" ]; then
    DIRECT=1
    shift
fi

function unbuffer() {
    stdbuf -i0 -o0 -e0 "$@"
}

# NAME
function run_component() {
    NAME="$1"
    shift
    echo "$NAME: Starting"
    if [ $DIRECT -eq 1 ]; then
        unbuffer dist/build/$NAME/$NAME
    else
        while read line; do
            echo "$NAME: $line"
        done < <(unbuffer dist/build/$NAME/$NAME "$@")
    fi
    echo "$NAME: Exited"
}

function run_component_bg() {
    run_component "$@" &
}

set +e
killall David Goliath Token &> /dev/null
set -e

run_component_bg Goliath "$@"
GPID=$!
sleep 0.2
run_component_bg Token "$@"
TPID=$!
sleep 0.5
run_component_bg David "$@"
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
