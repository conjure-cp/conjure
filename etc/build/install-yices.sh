#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ${BIN_DIR}/tmp-install-yices
mkdir -p ${BIN_DIR}/tmp-install-yices
pushd ${BIN_DIR}/tmp-install-yices

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

download https://github.com/SRI-CSL/yices2/archive/Yices-2.6.2.tar.gz
tar xzf Yices-2.6.2.tar.gz
cd yices2-Yices-2.6.2/
autoconf
./configure --prefix ${BIN_DIR}
make
make install
cp ${BIN_DIR}/bin/yices* ${BIN_DIR}
echo "yices executables are at ${BIN_DIR}/yices*"
ls -l ${BIN_DIR}/yices*
popd
rm -rf ${BIN_DIR}/tmp-install-yices
