#!/bin/bash

# version as of 25 March 2024
# alas, none of the published releases compile correctly
VERSION='3f923c08285bdc5186f995296e1c6bffa0588710'

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}


rm -rf tmp-install-runsolver
mkdir tmp-install-runsolver
pushd tmp-install-runsolver
git clone https://github.com/utpalbora/runsolver.git
cd runsolver
git checkout $VERSION
cd src
make -j${PROCESSES}
cp runsolver
echo "runsolver executable is at ${BIN_DIR}/runsolver"
ls -l ${BIN_DIR}/runsolver
popd
rm -rf tmp-install-runsolver

