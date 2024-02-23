#!/bin/bash

# version as of 16 November 2023
set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf tmp-install-minizinc
mkdir tmp-install-minizinc
pushd tmp-install-minizinc
git clone https://github.com/MiniZinc/libminizinc.git
cd libminizinc
mkdir build
cd build
cmake ..
cmake --build .
cp minizinc ${BIN_DIR}/minizinc
echo "minizinc executable is at ${BIN_DIR}/minizinc"
ls -l ${BIN_DIR}/minizinc
popd
rm -rf tmp-install-minizinc
