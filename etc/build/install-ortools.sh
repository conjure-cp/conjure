#!/bin/bash

# version as of Dec 2025
VERSION=v9.14

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export LIB_DIR=${LIB_DIR:-${HOME}/.local/lib}
export PROCESSES=${PROCESSES:-1}

mkdir -p ${BIN_DIR}
mkdir -p ${LIB_DIR}

rm -rf tmp-install-ortools
mkdir tmp-install-ortools
pushd tmp-install-ortools
git clone https://github.com/google/or-tools.git
cd or-tools
git checkout $VERSION
cmake -S . -B build -DBUILD_DEPS=ON -DUSE_COINOR=OFF
cmake --build build --config Release --target all -j${PROCESSES}
cp build/bin/fzn-cp-sat ${BIN_DIR}/fzn-cp-sat
# .dylib or .a depending on OS
cp build/lib*/lib* ${LIB_DIR}
echo "ortools executable is at ${BIN_DIR}/fzn-cp-sat"
ls -l ${BIN_DIR}/fzn-cp-sat
popd
rm -rf tmp-install-ortools
