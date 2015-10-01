#!/bin/bash

set -o errexit
set -o nounset

TESTCASE="$1"

if [ -d "${TESTCASE}" ]; then
    NUM=$(ls "${TESTCASE}/"*.essence | grep -v disabled.essence 2> /dev/null | wc -l)
    if [ "$NUM" -eq "1" ]; then
        echo "Accepting the output of ${TESTCASE}"
        touch "${TESTCASE}"/stdout "${TESTCASE}"/stderr
        cp "${TESTCASE}"/stdout "${TESTCASE}"/stdout.expected
        cp "${TESTCASE}"/stderr "${TESTCASE}"/stderr.expected
        find "${TESTCASE}" -size 0 -delete
    fi
fi
