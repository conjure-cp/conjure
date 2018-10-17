#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ${BIN_DIR}/tmp-install-open-wbo
mkdir ${BIN_DIR}/tmp-install-open-wbo
pushd ${BIN_DIR}/tmp-install-open-wbo
git clone https://github.com/sat-group/open-wbo.git
cd open-wbo
make r
cp open-wbo_release ${BIN_DIR}/open-wbo
echo "open-wbo executable is at ${BIN_DIR}/open-wbo"
ls -l ${BIN_DIR}/open-wbo
popd
rm -rf ${BIN_DIR}/tmp-install-open-wbo

