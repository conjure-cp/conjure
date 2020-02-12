#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ${BIN_DIR}/tmp-install-boolector
mkdir -p ${BIN_DIR}/tmp-install-boolector
pushd ${BIN_DIR}/tmp-install-boolector
git clone git@github.com:Boolector/boolector.git
cd boolector
git checkout 3.1.0
./contrib/setup-cadical.sh
./contrib/setup-btor2tools.sh
./configure.sh && cd build && make -j
cp bin/boolector ${BIN_DIR}/boolector
echo "boolector executable is at ${BIN_DIR}/boolector"
ls -l ${BIN_DIR}/boolector
popd
rm -rf ${BIN_DIR}/tmp-install-boolector
