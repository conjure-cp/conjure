#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ${BIN_DIR}/tmp-install-gecode
mkdir ${BIN_DIR}/tmp-install-gecode
pushd ${BIN_DIR}/tmp-install-gecode
git clone https://github.com/Gecode/gecode.git
cd gecode
git checkout release-6.0.1
mkdir build
cd build
../configure --disable-qt --disable-gist --enable-static
<<<<<<< HEAD
make -j
=======
make
>>>>>>> master
cp tools/flatzinc/fzn-gecode ${BIN_DIR}/fzn-gecode
echo "gecode executable is at ${BIN_DIR}/fzn-gecode"
ls -l ${BIN_DIR}/fzn-gecode
popd
rm -rf ${BIN_DIR}/tmp-install-gecode

