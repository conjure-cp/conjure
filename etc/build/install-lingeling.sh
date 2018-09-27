#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ${BIN_DIR}/tmp-install-lingeling
mkdir ${BIN_DIR}/tmp-install-lingeling
pushd ${BIN_DIR}/tmp-install-lingeling
wget --no-check-certificate -c http://fmv.jku.at/lingeling/lingeling-ayv-86bf266-140429.zip
unzip lingeling-ayv-86bf266-140429.zip
./build.sh
mkdir -p ${BIN_DIR}
cp binary/lingeling ${BIN_DIR}/lingeling
echo "lingeling executable is at ${BIN_DIR}/lingeling"
ls -l ${BIN_DIR}/lingeling
popd
rm -rf ${BIN_DIR}/tmp-install-lingeling

