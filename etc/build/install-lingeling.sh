#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ${BIN_DIR}/tmp-install-lingeling
mkdir -p ${BIN_DIR}/tmp-install-lingeling
pushd ${BIN_DIR}/tmp-install-lingeling

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

download http://fmv.jku.at/lingeling/lingeling-bbc-9230380-160707.tar.gz
tar xzf lingeling-bbc-9230380-160707.tar.gz
cd lingeling-bbc-9230380-160707
./configure.sh
make -j
mkdir -p ${BIN_DIR}
cp lingeling ${BIN_DIR}/lingeling
echo "lingeling executable is at ${BIN_DIR}/lingeling"
ls -l ${BIN_DIR}/lingeling
popd
rm -rf ${BIN_DIR}/tmp-install-lingeling

