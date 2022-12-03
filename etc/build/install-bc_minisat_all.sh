#!/bin/bash

# version as of 18 November 2022
VERSION=1.1.2

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}


rm -rf tmp-install-bc_minisat_all
mkdir -p tmp-install-bc_minisat_all
pushd tmp-install-bc_minisat_all

download http://www.sd.is.uec.ac.jp/toda/code/bc_minisat_all-$VERSION.tar.gz
tar zxf bc_minisat_all-$VERSION.tar.gz
cd bc_minisat_all-$VERSION/
make -j${PROCESSES} bc_minisat_all_release
ls -l
mv bc_minisat_all_release ${BIN_DIR}/bc_minisat_all_release
echo "bc_minisat_all executable is at ${BIN_DIR}/bc_minisat_all_release"
ls -l ${BIN_DIR}/bc_minisat_all_release
popd
rm -rf tmp-install-bc_minisat_all
