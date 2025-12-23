#!/bin/bash

# version as of 18 November 2022
VERSION=2.6.4

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}

# we will add these to the path, just in case the user is using a homebrew installed gmp
export C_INCLUDE_PATH=${C_INCLUDE_PATH:-} # initialise if undefined
export C_INCLUDE_PATH=/opt/homebrew/include:${C_INCLUDE_PATH} # add the brew dir
export CPLUS_INCLUDE_PATH=${CPLUS_INCLUDE_PATH:-}
export CPLUS_INCLUDE_PATH=/opt/homebrew/include:${CPLUS_INCLUDE_PATH}
export LIBRARY_PATH=${LIBRARY_PATH:-}
export LIBRARY_PATH=/opt/homebrew/lib/:${LIBRARY_PATH}

rm -rf tmp-install-yices
mkdir -p tmp-install-yices
pushd tmp-install-yices

download https://github.com/SRI-CSL/yices2/archive/Yices-$VERSION.tar.gz
tar xzf Yices-$VERSION.tar.gz
cd yices2-Yices-$VERSION/
autoconf
./configure --prefix ${BIN_DIR}
make -j${PROCESSES}
make -j${PROCESSES} install
mv ${BIN_DIR}/bin/yices* ${BIN_DIR}
echo "yices executables are at ${BIN_DIR}/yices*"
ls -l ${BIN_DIR}/yices*
popd
rm -rf tmp-install-yices
