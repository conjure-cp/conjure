#!/bin/bash

# version as of 18 November 2022
VERSION=bcj-78ebb86-180517

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}

arch="$(uname -m)"
if [ "${arch}" = "aarch64" ] || [ "${arch}" = "arm64" ]; then
    # Clang on arm64 avoids Lingeling watcher stack overflows seen with GCC builds.
    export CC=clang
    export CXX=clang++
fi


rm -rf tmp-install-lingeling
mkdir -p tmp-install-lingeling
pushd tmp-install-lingeling

download http://fmv.jku.at/lingeling/lingeling-$VERSION.tar.gz
tar xzf lingeling-$VERSION.tar.gz
rm -f lingeling-$VERSION.tar.gz
cd lingeling-$VERSION
./configure.sh
make -j${PROCESSES}
mkdir -p ${BIN_DIR}
cp lingeling ${BIN_DIR}/lingeling
cp plingeling ${BIN_DIR}/plingeling
cp treengeling ${BIN_DIR}/treengeling
echo "lingeling executable is at ${BIN_DIR}/lingeling"
echo "plingeling executable is at ${BIN_DIR}/plingeling"
echo "treengeling executable is at ${BIN_DIR}/treengeling"
ls -l ${BIN_DIR}/lingeling ${BIN_DIR}/plingeling ${BIN_DIR}/treengeling
popd
rm -rf tmp-install-lingeling
