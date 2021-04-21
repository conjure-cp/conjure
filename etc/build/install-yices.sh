#!/bin/bash

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}


rm -rf tmp-install-yices
mkdir -p tmp-install-yices
pushd tmp-install-yices

download https://github.com/SRI-CSL/yices2/archive/Yices-2.6.2.tar.gz
tar xzf Yices-2.6.2.tar.gz
cd yices2-Yices-2.6.2/
autoconf
./configure --prefix ${BIN_DIR}
make -j${PROCESSES}
make -j${PROCESSES} install
cp ${BIN_DIR}/bin/yices* ${BIN_DIR}
echo "yices executables are at ${BIN_DIR}/yices*"
ls -l ${BIN_DIR}/yices*
popd
rm -rf tmp-install-yices
