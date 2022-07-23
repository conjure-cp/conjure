#!/bin/bash

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf tmp-install-parallel
mkdir -p tmp-install-parallel
pushd tmp-install-parallel
download https://ftpmirror.gnu.org/parallel/parallel-latest.tar.bz2
tar -xvjf parallel-latest.tar.bz2
cp parallel-*/src/parallel ${BIN_DIR}/parallel
echo "parallel executable is at ${BIN_DIR}/parallel"
ls -l ${BIN_DIR}/parallel
popd
rm -rf tmp-install-parallel
