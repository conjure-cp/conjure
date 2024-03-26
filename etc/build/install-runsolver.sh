#!/bin/bash

# version as of 25 March 2024
# alas, none of the published releases compile correctly
VERSION='3f923c08285bdc5186f995296e1c6bffa0588710'

set -o errexit
set -o nounset

DIR="$( cd "$( dirname "$0" )" && pwd )"

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}

OS=$(uname)

if [ "$OS" == "Linux" ]; then
    rm -rf tmp-install-runsolver
    mkdir tmp-install-runsolver
    pushd tmp-install-runsolver
    git clone https://github.com/utpalbora/runsolver.git
    cd runsolver
    git checkout $VERSION
    cd src
    if make -j${PROCESSES} ; then
        echo "Built."
    else
        echo "Build failed, trying again with a local patch"
        echo "This might happen when building with an old version of libnuma-dev"
        patch runsolver.cc ${DIR}/etc/build/runsolver.patch
        make -j${PROCESSES}
    fi
    cp runsolver ${BIN_DIR}/runsolver
    echo "runsolver executable is at ${BIN_DIR}/runsolver"
    ls -l ${BIN_DIR}/runsolver
    popd
    rm -rf tmp-install-runsolver
else
    echo "Your OS is (according to uname): ${OS} -- runsolver only works on linux."
fi
