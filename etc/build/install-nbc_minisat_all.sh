#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ${BIN_DIR}/tmp-install-nbc_minisat_all
mkdir ${BIN_DIR}/tmp-install-nbc_minisat_all
pushd ${BIN_DIR}/tmp-install-nbc_minisat_all
wget --no-check-certificate -c http://www.sd.is.uec.ac.jp/toda/code/nbc_minisat_all-1.0.2.tar.gz
tar zxf nbc_minisat_all-1.0.2.tar.gz
cd nbc_minisat_all-1.0.2/
make -j nbc_minisat_all_release
mv nbc_minisat_all_release ${BIN_DIR}/nbc_minisat_all_release
echo "nbc_minisat_all executable is at ${BIN_DIR}/nbc_minisat_all_release"
ls -l ${BIN_DIR}/nbc_minisat_all_release
popd
rm -rf ${BIN_DIR}/tmp-install-nbc_minisat_all

