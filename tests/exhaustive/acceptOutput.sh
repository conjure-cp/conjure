#!/bin/bash

set -o errexit
set -o nounset

TESTCASE="$1"

if [ -d "${TESTCASE}" ]; then
    NUM_ESSENCE=$(ls "${TESTCASE}/"*.essence 2> /dev/null | wc -l | tr -d ' ')
    NUM_INVALID=$(ls "${TESTCASE}/invalid" 2> /dev/null | wc -l | tr -d ' ')
    if [ "$NUM_ESSENCE" -eq "1" ] && [ "$NUM_INVALID" -eq 0 ]; then
        echo "Accepting the output of ${TESTCASE}"
        rm -f "${TESTCASE}"/expected/*
        mkdir -p "${TESTCASE}"/expected
        cp "${TESTCASE}"/outputs/*.eprime \
           "${TESTCASE}"/outputs/*.solution \
           "${TESTCASE}"/outputs/*.eprime-param \
           "${TESTCASE}"/expected/ 2> /dev/null || :
        parallel --no-notice "[ -f {} ] && (cat {} | grep -v '\\$' > {}.temp ; mv {}.temp {})" \
            ::: "${TESTCASE}"/expected/*.eprime
    fi
fi
