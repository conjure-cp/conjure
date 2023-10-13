#!/bin/bash

# version as of 18 November 2022
VERSION=80f3073e41028b219b0b0ad7c61fba28351f88e6

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}

# we will add these to the path, just in case the user is using a homebrew installed gmp
export C_INCLUDE_PATH=${C_INCLUDE_PATH:-} # initialise if undefined
export C_INCLUDE_PATH=/opt/homebrew/include:${C_INCLUDE_PATH} # add the brew dir
export CPLUS_INCLUDE_PATH=${CPLUS_INCLUDE_PATH:-}
export CPLUS_INCLUDE_PATH=/opt/homebrew/include:${CPLUS_INCLUDE_PATH}
export LIBRARY_PATH=${LIBRARY_PATH:-}
export LIBRARY_PATH=/opt/homebrew/lib/:${LIBRARY_PATH}

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

