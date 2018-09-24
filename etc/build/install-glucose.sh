#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ~/tmp-install-glucose
mkdir ~/tmp-install-glucose
pushd ~/tmp-install-glucose
wget http://www.labri.fr/perso/lsimon/downloads/softwares/glucose-syrup-4.1.tgz
tar -xvzf glucose-syrup-4.1.tgz
cd glucose-syrup-4.1/
cd parallel
make r
cp glucose-syrup_release ${BIN_DIR}/glucose-syrup
echo "glucose executable is at ${BIN_DIR}/glucose-syrup"
ls -l ${BIN_DIR}/glucose-syrup
popd
rm -rf ~/tmp-install-glucose

