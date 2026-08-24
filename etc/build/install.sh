#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export LIB_DIR=${BIN_DIR}/lib
export CI=${CI:-false}
export BUILD_TESTS=${BUILD_TESTS:-false}
export COVERAGE=${COVERAGE:-false}

COMMAND="stack install --local-bin-path ${BIN_DIR}"

if  [ ${GHC_VERSION} == "head" ]; then
    COMMAND="${COMMAND} --resolver nightly"
fi

if ${BUILD_TESTS}; then
    rm -f .stack-work/dist/*/*/build/conjure-testing/conjure-testing
    # stack decides what to relink from its own build cache, not from whether the
    # executable is still there, so drop the cache entry as well. without this,
    # deleting the executable when nothing else is dirty leaves us with no
    # executable at all: stack sees the test component as up to date and skips it.
    rm -f .stack-work/dist/*/*/stack-build-caches/*/test-conjure-testing
    COMMAND="${COMMAND} --test --no-run-tests"
fi

if ${CI}; then
    COMMAND="${COMMAND} --no-terminal"
fi

if ${COVERAGE}; then
    COMMAND="${COMMAND} --coverage"
fi

echo "Running: ${COMMAND}"
${COMMAND}
