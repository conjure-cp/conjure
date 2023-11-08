#!/bin/bash

export BRANCH=$(git rev-parse --abbrev-ref HEAD)

# make a copy of the executable and call it conjure-${BRANCH}
if [ "x${BRANCH}" != 'xmain' -a "x${BRANCH}" != 'xHEAD' -a "x${BRANCH}" != 'x' ]; then
	cp ${BIN_DIR}/conjure ${BIN_DIR}/conjure-${BRANCH}
    echo "- conjure-${BRANCH}"
fi
