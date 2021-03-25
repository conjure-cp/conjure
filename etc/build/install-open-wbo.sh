#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf tmp-install-open-wbo
mkdir -p tmp-install-open-wbo
pushd tmp-install-open-wbo
git clone https://github.com/sat-group/open-wbo.git
cd open-wbo
make -j r
cp open-wbo_release ${BIN_DIR}/open-wbo
echo "open-wbo executable is at ${BIN_DIR}/open-wbo"
ls -l ${BIN_DIR}/open-wbo
popd
rm -rf tmp-install-open-wbo

