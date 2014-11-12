#!/bin/bash

set -o errexit
set -o nounset

TESTCASE="$1"

echo "Accepting the output of ${TESTCASE}"
rm -f "${TESTCASE}"/expected/*
cp "${TESTCASE}"/outputs/*.eprime "${TESTCASE}"/expected/
parallel "cat {} | grep -v '\\$' > {}.temp ; mv {}.temp {}" ::: "${TESTCASE}"/expected/*.eprime
cp "${TESTCASE}"/outputs/*.solution "${TESTCASE}"/expected/
