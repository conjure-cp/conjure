#!/bin/bash

# exit silently if we are not inside a git repository
if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    exit 0
fi

export BRANCH=$(git rev-parse --abbrev-ref HEAD)

# make a copy of the executable and call it conjure-${BRANCH}
if [ "x${BRANCH}" != 'xmain' -a "x${BRANCH}" != 'xHEAD' -a "x${BRANCH}" != 'x' ]; then
	cp ${BIN_DIR}/conjure ${BIN_DIR}/conjure-${BRANCH}
    echo "- conjure-${BRANCH}"
fi
