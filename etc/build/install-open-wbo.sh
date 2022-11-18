#!/bin/bash

# version as of 18 November 2022
VERSION=80f3073e41028b219b0b0ad7c61fba28351f88e6

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}


rm -rf tmp-install-open-wbo
mkdir -p tmp-install-open-wbo
pushd tmp-install-open-wbo
git clone https://github.com/sat-group/open-wbo.git
cd open-wbo
git checkout $VERSION
make -j${PROCESSES} r
cp open-wbo_release ${BIN_DIR}/open-wbo
echo "open-wbo executable is at ${BIN_DIR}/open-wbo"
ls -l ${BIN_DIR}/open-wbo
popd
rm -rf tmp-install-open-wbo

