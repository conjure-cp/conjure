#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ${BIN_DIR}/tmp-install-z3
mkdir -p ${BIN_DIR}/tmp-install-z3
pushd ${BIN_DIR}/tmp-install-z3

function download {
    if which curl 2> /dev/null > /dev/null; then
        curl -L -O $1
    elif which wget 2> /dev/null > /dev/null; then
        wget --no-check-certificate -c $1
    else
        echo "You seem to have neither curl nor wget on this computer."
        echo "Cannot download without one of them."
        exit 1
    fi
}
export -f download

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
