#!/bin/bash

function list_all_essence() {
    find * -name "*.essence"
}
export -f list_all_essence

function do_ts1() {
    ESSENCE="$1"
    conjure --mode typeStrengthening --in "${ESSENCE}" > "${ESSENCE}.typeStrengthening"
    conjure --mode diff "${ESSENCE}.typeStrengthening" "${ESSENCE}.expected" 2> /dev/null
    if [ $? -eq 0 ] ; then
        echo "pass ${ESSENCE}"
        rm "${ESSENCE}.typeStrengthening"
    else
        echo "fail ${ESSENCE}"
    fi
}
export -f do_ts1

function do_ts() {
    parallel --no-notice -k do_ts1 ::: $(list_all_essence)
}
export -f do_ts

function rm_output_files() {
    find . -name "*.typeStrengthening" -delete
}
export -f do_ts

function recompute() {
    do_ts | tee stdout
    NBFAIL=$(grep fail stdout | wc -l | tr -d ' ')
    NBPASS=$(grep pass stdout | wc -l | tr -d ' ')
    echo "Number of failing tests: ${NBFAIL}"
    echo "Number of passing tests: ${NBPASS}"
    rm stdout
}
export -f recompute

