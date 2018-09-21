#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ~/tmp-install-chuffed
mkdir ~/tmp-install-chuffed
pushd ~/tmp-install-chuffed
git clone https://github.com/chuffed/chuffed.git
cd chuffed
mkdir build
cd build
cmake ..
cmake --build .
cp fzn-chuffed ${BIN_DIR}/fzn-chuffed
echo "chuffed executable is at ${BIN_DIR}/fzn-chuffed"
ls -l ${BIN_DIR}/fzn-chuffed
popd
rm -rf ~/tmp-install-chuffed

