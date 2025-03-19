#!/bin/bash

# version as of 3 June 2024
VERSION=2.8.5
set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}

rm -rf tmp-install-minizinc
mkdir tmp-install-minizinc
pushd tmp-install-minizinc
git clone https://github.com/MiniZinc/libminizinc.git
cd libminizinc
git checkout $VERSION
mkdir build
cd build
cmake ..
cmake --build . -j${PROCESSES}
cp minizinc ${BIN_DIR}/minizinc
mkdir -p ${BIN_DIR}/share
cp -r ../share/minizinc ${BIN_DIR}/share/minizinc
echo "minizinc executable is at ${BIN_DIR}/minizinc"
ls -l ${BIN_DIR}/minizinc
export MZN_STDLIB_DIR=${BIN_DIR}/share/minizinc/
echo "set env variable MZN_STDLIB_DIR to ${BIN_DIR}/share/minizinc. Please consider making it permanent"
popd
rm -rf tmp-install-minizinc
