#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ${BIN_DIR}/tmp-install-glucose
mkdir -p ${BIN_DIR}/tmp-install-glucose
pushd ${BIN_DIR}/tmp-install-glucose

function download {
    if which curl 2> /dev/null > /dev/null; then
        curl $1 -O
    elif which wget 2> /dev/null > /dev/null; then
        wget --no-check-certificate -c $1
    else
        echo "You seem to have neither curl nor wget on this computer."
        echo "Cannot download without one of them."
        exit 1
    fi
}
export -f download

download http://www.labri.fr/perso/lsimon/downloads/softwares/glucose-syrup-4.1.tgz
tar zxf glucose-syrup-4.1.tgz
cd glucose-syrup-4.1/
(
    cd simp
    make -j r
    cp glucose_release ${BIN_DIR}/glucose
    echo "glucose executable is at ${BIN_DIR}/glucose"
    ls -l ${BIN_DIR}/glucose
)
(
    cd parallel
    make -j r
    cp glucose-syrup_release ${BIN_DIR}/glucose-syrup
    echo "glucose-syrup executable is at ${BIN_DIR}/glucose-syrup"
    ls -l ${BIN_DIR}/glucose-syrup
)
popd
rm -rf ${BIN_DIR}/tmp-install-glucose

