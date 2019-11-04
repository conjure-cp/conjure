#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ${BIN_DIR}/tmp-install-nbc_minisat_all
mkdir -p ${BIN_DIR}/tmp-install-nbc_minisat_all
pushd ${BIN_DIR}/tmp-install-nbc_minisat_all

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

download http://www.sd.is.uec.ac.jp/toda/code/nbc_minisat_all-1.0.2.tar.gz
tar zxf nbc_minisat_all-1.0.2.tar.gz
cd nbc_minisat_all-1.0.2/
make -j nbc_minisat_all_release
mv nbc_minisat_all_release ${BIN_DIR}/nbc_minisat_all_release
echo "nbc_minisat_all executable is at ${BIN_DIR}/nbc_minisat_all_release"
ls -l ${BIN_DIR}/nbc_minisat_all_release
popd
rm -rf ${BIN_DIR}/tmp-install-nbc_minisat_all

