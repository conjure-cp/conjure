#!/bin/bash

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf tmp-install-nbc_minisat_all
mkdir -p tmp-install-nbc_minisat_all
pushd tmp-install-nbc_minisat_all

download http://www.sd.is.uec.ac.jp/toda/code/nbc_minisat_all-1.0.2.tar.gz
tar zxf nbc_minisat_all-1.0.2.tar.gz
cd nbc_minisat_all-1.0.2/
make -j nbc_minisat_all_release
mv nbc_minisat_all_release ${BIN_DIR}/nbc_minisat_all_release
echo "nbc_minisat_all executable is at ${BIN_DIR}/nbc_minisat_all_release"
ls -l ${BIN_DIR}/nbc_minisat_all_release
popd
rm -rf tmp-install-nbc_minisat_all

