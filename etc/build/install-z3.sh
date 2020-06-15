#!/bin/bash

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ${BIN_DIR}/tmp-install-z3
mkdir -p ${BIN_DIR}/tmp-install-z3
pushd ${BIN_DIR}/tmp-install-z3

download https://github.com/Z3Prover/z3/archive/z3-4.8.8.tar.gz
tar xzf z3-4.8.8.tar.gz
cd z3-z3-4.8.8
python scripts/mk_make.py --prefix=${BIN_DIR}
cd build
make -j
make -j install
cp ${BIN_DIR}/bin/z3 ${BIN_DIR}
echo "z3 executable is at ${BIN_DIR}/z3"
ls -l ${BIN_DIR}/z3
popd
rm -rf ${BIN_DIR}/tmp-install-z3
