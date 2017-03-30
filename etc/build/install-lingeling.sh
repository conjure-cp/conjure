#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.cabal/bin}

rm -rf ~/tmp-install-lingeling
mkdir ~/tmp-install-lingeling
pushd ~/tmp-install-lingeling
wget -c http://fmv.jku.at/lingeling/lingeling-ayv-86bf266-140429.zip
unzip lingeling-ayv-86bf266-140429.zip
./build.sh
mkdir -p ${BIN_DIR}
cp binary/lingeling ${BIN_DIR}/lingeling
echo "lingeling executable is at ${BIN_DIR}/lingeling"
ls -l ${BIN_DIR}/lingeling
popd
rm -rf ~/tmp-install-lingeling

