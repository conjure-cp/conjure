#!/bin/bash

set -o errexit
set -o nounset

TESTCASE="$1"

if [ -d "${TESTCASE}" ]; then
    NUM=$(ls "${TESTCASE}/"*.essence 2> /dev/null | grep -v disabled.essence | wc -l | tr -d ' ')
    if [ "$NUM" -eq "1" ]; then
        echo "Accepting the output of ${TESTCASE}"
        touch "${TESTCASE}"/model.json "${TESTCASE}"/stderr
        cp "${TESTCASE}"/model.json "${TESTCASE}"/model.expected.json
        cp "${TESTCASE}"/stderr "${TESTCASE}"/stderr.expected
        find \
            "${TESTCASE}"/model.json \
            "${TESTCASE}"/stderr \
            "${TESTCASE}"/model.expected.json \
            "${TESTCASE}"/stderr.expected \
            -size 0 \
            -exec rm {} \;
    fi
fi
