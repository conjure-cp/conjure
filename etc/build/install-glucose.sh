#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ${BIN_DIR}/tmp-install-glucose
mkdir ${BIN_DIR}/tmp-install-glucose
pushd ${BIN_DIR}/tmp-install-glucose
wget --no-check-certificate -c http://www.labri.fr/perso/lsimon/downloads/softwares/glucose-syrup-4.1.tgz
tar zxf glucose-syrup-4.1.tgz
cd glucose-syrup-4.1/
cd parallel
make -j r
cp glucose-syrup_release ${BIN_DIR}/glucose-syrup
echo "glucose executable is at ${BIN_DIR}/glucose-syrup"
ls -l ${BIN_DIR}/glucose-syrup
popd
rm -rf ${BIN_DIR}/tmp-install-glucose

