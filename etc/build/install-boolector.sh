#!/bin/bash

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}


rm -rf tmp-install-boolector
mkdir -p tmp-install-boolector
pushd tmp-install-boolector

download https://github.com/Boolector/boolector/archive/3.2.1.tar.gz
tar xzf 3.2.1.tar.gz
cd boolector-3.2.1
./contrib/setup-lingeling.sh
./contrib/setup-btor2tools.sh
./configure.sh && cd build && make -j${PROCESSES}
cp bin/boolector ${BIN_DIR}
echo "boolector executable is at ${BIN_DIR}/boolector"
ls -l ${BIN_DIR}/boolector
popd
rm -rf tmp-install-boolector
