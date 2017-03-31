#!/bin/bash

# Default values for environment variables (to be used by the install script and make)

export CORES=${CORES:-0}
export GHC_VERSION=${GHC_VERSION:-"8.0.1"}
export GMP_VERSION=${GMP_VERSION:-"NEWGMP"}
export INSTALL_GMP=${INSTALL_GMP:-no}
export INSTALL_GHC=${INSTALL_GHC:-no}
export INSTALL_CABAL=${INSTALL_CABAL:-no}
export OPTIMISATION=${OPTIMISATION:-"-O2"}
export LLVM=${LLVM:-"llvm-off"}
export BIN_DIR=${BIN_DIR:-${HOME}/.cabal/bin}
export BUILD_DOCS=${BUILD_DOCS:-no}
export BUILD_TESTS=${BUILD_TESTS:-no}
export RUN_TESTS=${RUN_TESTS:-no}
export COVERAGE=${COVERAGE:-no}
export PROFILING=${PROFILING:-no}
export DYNAMIC=${DYNAMIC:-no}
export SPLIT_OBJS=${SPLIT_OBJS:-no}
export DEVELOPMENT_MODE=${DEVELOPMENT_MODE:-no}

export PATH="${HOME}/.tools/ghc/${GHC_VERSION}/bin":$PATH
export PATH="${HOME}/.cabal/bin":$PATH
export PATH="${BIN_DIR}":$PATH
