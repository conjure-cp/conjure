#!/bin/bash

# version as of December 2025
VERSION=5c9f347c864bc2b64684b1df9d366294c21aac49

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}


rm -rf tmp-install-minion
mkdir -p tmp-install-minion
pushd tmp-install-minion
git clone https://github.com/minion/minion.git
cd minion
git checkout ${VERSION}
mkdir build
cd build
../configure.py
make -j${PROCESSES} minion
cp minion ${BIN_DIR}/minion
echo "minion executable is at ${BIN_DIR}/minion"
ls -l ${BIN_DIR}/minion*
popd
rm -rf tmp-install-minion
