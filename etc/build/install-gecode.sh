#!/bin/bash

# version as of 16 February 2023
# alas, none of the published releases compile correctly
VERSION=f2ce8db2fdb6cf2d357059e8959bb77442826ed6

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}


rm -rf tmp-install-gecode
mkdir tmp-install-gecode
pushd tmp-install-gecode
git clone https://github.com/Gecode/gecode.git
cd gecode
git checkout $VERSION
mkdir build
cd build
../configure --disable-qt --disable-gist --enable-static
make -j${PROCESSES}
cp tools/flatzinc/fzn-gecode ${BIN_DIR}/fzn-gecode
echo "gecode executable is at ${BIN_DIR}/fzn-gecode"
ls -l ${BIN_DIR}/fzn-gecode
popd
rm -rf tmp-install-gecode

