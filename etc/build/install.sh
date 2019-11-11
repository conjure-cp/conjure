#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export CI=${CI:-false}
export BUILD_TESTS=${BUILD_TESTS:-false}
export COVERAGE=${COVERAGE:-false}

COMMAND="stack install --local-bin-path ${BIN_DIR}"
echo $COMMAND

if  [ ${GHC_VERSION} == "head" ]; then
    COMMAND="${COMMAND} --resolver nightly"
    echo $COMMAND
fi

if ${BUILD_TESTS}; then
    COMMAND="${COMMAND} --test --no-run-tests"
    echo $COMMAND
fi

if ${CI}; then
    COMMAND="${COMMAND} --no-terminal"
    echo $COMMAND
fi

if ${COVERAGE}; then
    COMMAND="${COMMAND} --coverage"
    echo $COMMAND
fi

echo "Running: ${COMMAND}"
${COMMAND}
