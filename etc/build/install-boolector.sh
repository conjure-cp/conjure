#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ${BIN_DIR}/tmp-install-boolector
mkdir -p ${BIN_DIR}/tmp-install-boolector
pushd ${BIN_DIR}/tmp-install-boolector

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

download https://github.com/Boolector/boolector/archive/3.2.1.tar.gz
tar xzf 3.2.1.tar.gz
cd boolector-3.2.1
./contrib/setup-lingeling.sh
./contrib/setup-btor2tools.sh
./configure.sh && cd build && make
cp bin/boolector ${BIN_DIR}
echo "boolector executable is at ${BIN_DIR}/boolector"
ls -l ${BIN_DIR}/boolector
popd
rm -rf ${BIN_DIR}/tmp-install-boolector
