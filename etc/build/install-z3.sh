#!/bin/bash

# version as of 18 November 2022
VERSION=4.11.2

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}


rm -rf tmp-install-z3
mkdir -p tmp-install-z3
pushd tmp-install-z3

download https://github.com/Z3Prover/z3/archive/z3-$VERSION.tar.gz
tar xzf z3-$VERSION.tar.gz
cd z3-z3-$VERSION
PYTHON=`command -v python 2> /dev/null || true`
PYTHON=${PYTHON:-python3}
$PYTHON scripts/mk_make.py --prefix=${BIN_DIR}
cd build
make -j${PROCESSES}
make -j${PROCESSES} install
cp ${BIN_DIR}/bin/z3 ${BIN_DIR}
echo "z3 executable is at ${BIN_DIR}/z3"
ls -l ${BIN_DIR}/z3
popd
rm -rf tmp-install-z3
