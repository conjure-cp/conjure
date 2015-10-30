#!/bin/bash

set -o errexit
set -o nounset

TESTCASE="$1"

if [ -d "${TESTCASE}" ]; then
    NUM=$(ls "${TESTCASE}/"*.essence 2> /dev/null | grep -v disabled.essence | wc -l | tr -d ' ')
    if [ "$NUM" -eq "1" ]; then
        echo "Accepting the output of ${TESTCASE}"
        rm -f "${TESTCASE}"/expected/*
        mkdir -p "${TESTCASE}"/expected
        cp "${TESTCASE}"/outputs/*.eprime \
           "${TESTCASE}"/outputs/*.solution \
           "${TESTCASE}"/outputs/*.eprime-param \
           "${TESTCASE}"/expected/ 2> /dev/null || :
        parallel "[ -f {} ] && (cat {} | grep -v '\\$' > {}.temp ; mv {}.temp {})" ::: "${TESTCASE}"/expected/*.eprime
    fi
fi
