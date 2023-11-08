#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export CI=${CI:-false}
export BUILD_TESTS=${BUILD_TESTS:-false}
export COVERAGE=${COVERAGE:-false}

COMMAND="stack install --local-bin-path ${BIN_DIR}"

if  [ ${GHC_VERSION} == "head" ]; then
    COMMAND="${COMMAND} --resolver nightly"
fi

if ${BUILD_TESTS}; then
    rm -f .stack-work/dist/*/*/build/conjure-testing/conjure-testing
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
