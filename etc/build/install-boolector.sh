#!/bin/bash

# version as of May 2025
VERSION=3.2.4

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

set -o errexit
set -o nounset

DIR="$( cd "$( dirname "$0" )" && pwd )"

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}


rm -rf tmp-install-boolector
mkdir -p tmp-install-boolector
pushd tmp-install-boolector

download https://github.com/Boolector/boolector/archive/$VERSION.tar.gz
tar xzf $VERSION.tar.gz
cd boolector-$VERSION
patch -p1 < ${DIR}/boolector-cmake.patch
grep cmake -R .
./contrib/setup-cadical.sh
./contrib/setup-btor2tools.sh
./configure.sh
cd build && make -j${PROCESSES}
cp bin/boolector ${BIN_DIR}
echo "boolector executable is at ${BIN_DIR}/boolector"
ls -l ${BIN_DIR}/boolector
popd
rm -rf tmp-install-boolector
