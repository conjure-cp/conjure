#!/bin/bash

# version as of Dec 2025
VERSION=v9.14

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

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
download https://github.com/google/or-tools/archive/refs/tags/${VERSION}.zip
unzip ${VERSION}.zip
cd or-tools-*
cmake -S . -B build -DBUILD_DEPS=ON
cmake --build build --config Release --target all -j${PROCESSES}
cp build/bin/fzn-cp-sat ${BIN_DIR}/fzn-cp-sat
# .dylib or .a depending on OS
cp build/lib*/lib* ${LIB_DIR}
echo "ortools executable is at ${BIN_DIR}/fzn-cp-sat"
ls -l ${BIN_DIR}/fzn-cp-sat
popd
rm -rf tmp-install-ortools
