#!/bin/bash

set -o errexit
set -o nounset

TESTCASE="$1"

if [ -d "${TESTCASE}" ]; then
    echo "Accepting the output of ${TESTCASE}"
    touch "${TESTCASE}"/stdout "${TESTCASE}"/stderr
    mv "${TESTCASE}"/stdout "${TESTCASE}"/stdout.expected
    mv "${TESTCASE}"/stderr "${TESTCASE}"/stderr.expected
    find "${TESTCASE}" -size 0 -delete
fi
