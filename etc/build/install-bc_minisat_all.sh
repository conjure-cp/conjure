#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ${BIN_DIR}/tmp-install-bc_minisat_all
mkdir ${BIN_DIR}/tmp-install-bc_minisat_all
pushd ${BIN_DIR}/tmp-install-bc_minisat_all
wget --no-check-certificate -c http://www.sd.is.uec.ac.jp/toda/code/bc_minisat_all-1.1.2.tar.gz
tar zxf bc_minisat_all-1.1.2.tar.gz
cd bc_minisat_all-1.1.2/
make bc_minisat_all_release
mv bc_minisat_all_release ${BIN_DIR}/bc_minisat_all_release
echo "bc_minisat_all executable is at ${BIN_DIR}/bc_minisat_all_release"
ls -l ${BIN_DIR}/bc_minisat_all_release
popd
rm -rf ${BIN_DIR}/tmp-install-bc_minisat_all

