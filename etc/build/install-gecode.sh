#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ~/tmp-install-gecode
mkdir ~/tmp-install-gecode
pushd ~/tmp-install-gecode
git clone https://github.com/Gecode/gecode.git
cd gecode
mkdir build
cd build
cmake ..
cmake --build .
cp bin/fzn-gecode ${BIN_DIR}/fzn-gecode
echo "gecode executable is at ${BIN_DIR}/fzn-gecode"
ls -l ${BIN_DIR}/fzn-gecode
popd
rm -rf ~/tmp-install-gecode

