#!/bin/bash

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf tmp-install-bc_minisat_all
mkdir -p tmp-install-bc_minisat_all
pushd tmp-install-bc_minisat_all

download http://www.sd.is.uec.ac.jp/toda/code/bc_minisat_all-1.1.2.tar.gz
tar zxf bc_minisat_all-1.1.2.tar.gz
cd bc_minisat_all-1.1.2/
make -j bc_minisat_all_release
mv bc_minisat_all_release ${BIN_DIR}/bc_minisat_all_release
echo "bc_minisat_all executable is at ${BIN_DIR}/bc_minisat_all_release"
ls -l ${BIN_DIR}/bc_minisat_all_release
popd
rm -rf tmp-install-bc_minisat_all

