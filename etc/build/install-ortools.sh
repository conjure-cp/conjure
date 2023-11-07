#!/bin/bash

# version as of 7 Nov 2023
VERSION=v9.7

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf tmp-install-ortools
mkdir tmp-install-ortools
pushd tmp-install-ortools
download https://github.com/google/or-tools/archive/refs/tags/${VERSION}.zip
unzip ${VERSION}.zip
cd or-tools-9.7
cmake -S. -Bbuild -DBUILD_DEPS:BOOL=ONere
cmake --build build
cp build/bin/fzn-ortools ${BIN_DIR}/fzn-ortools
cp build/lib/libortools_flatzinc.9.dylib build/lib/libortools.9.dylib ${BIN_DIR}
echo "ortools executable is at ${BIN_DIR}/fzn-ortools"
ls -l ${BIN_DIR}/fzn-ortools
popd
rm -rf tmp-install-ortools
