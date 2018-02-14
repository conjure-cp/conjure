#!/bin/bash

set -o errexit
set -o nounset

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source ${SCRIPT_DIR}/default_envvars.sh

rm -rf dist \
       cabal.sandbox.config .cabal-sandbox \
       .stack-work \
       src/RepositoryVersion.hs \
       cabal.config \
       etc/hs-deps/cabal.config-${GHC_VERSION}

find . -name "*.prof" -delete
find . -name "*.hi" -delete
find . -name "*.o" -delete
find . -name "*.hi-boot" -delete
find . -name "*.o-boot" -delete
