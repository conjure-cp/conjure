#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ${BIN_DIR}/tmp-install-cadical
mkdir -p ${BIN_DIR}/tmp-install-cadical
pushd ${BIN_DIR}/tmp-install-cadical

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

download https://github.com/arminbiere/cadical/archive/rel-1.3.0.tar.gz
tar xzf rel-1.3.0.tar.gz
cd cadical-rel-1.3.0
./configure
make -j
mkdir -p ${BIN_DIR}
cp build/cadical ${BIN_DIR}/cadical
echo "cadical executable is at ${BIN_DIR}/cadical"
ls -l ${BIN_DIR}/cadical
popd
rm -rf ${BIN_DIR}/tmp-install-cadical

