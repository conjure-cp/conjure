#/bin/bash

# This script will install ghc + cabal + happy + conjure.
# It will only install these tools if they aren't already installed.
#
# It uses a cabal-sanbox, so dependencies are installed locally.
# GHC will be installed under ~/.tools/ghc/${GHC_VERSION}
# cabal-install'ed executables will be under ~/.cabal/bin as usual.
# 
# To clean an installation:
#     rm -rf dist ~/.cabal-sandbox cabal.sandbox.config


set -o errexit
set -o nounset

export CORES=${CORES:-0}
export GHC_VERSION=${GHC_VERSION:-"7.6.3"}
export OPTIMISATION=${OPTIMISATION:-"-O2"}
export LLVM=${LLVM:-"llvm-off"}
export BIN_DIR=${BIN_DIR:-${HOME}/.cabal/bin}

# where is this script?
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"

bash "${SCRIPT_DIR}/install_jenkins.sh"
