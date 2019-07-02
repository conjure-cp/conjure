#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ${BIN_DIR}/tmp-install-chuffed
mkdir ${BIN_DIR}/tmp-install-chuffed
pushd ${BIN_DIR}/tmp-install-chuffed
git clone https://github.com/chuffed/chuffed.git
cd chuffed
git checkout 0.10.2
# git submodule update --init     # cp-profiler support
mkdir build
cd build
cmake ..
cmake --build .
cp fzn-chuffed ${BIN_DIR}/fzn-chuffed
echo "chuffed executable is at ${BIN_DIR}/fzn-chuffed"
ls -l ${BIN_DIR}/fzn-chuffed
popd
rm -rf ${BIN_DIR}/tmp-install-chuffed

