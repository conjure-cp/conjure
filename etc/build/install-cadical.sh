#!/bin/bash

# version as of 18 November 2022
VERSION=1.5.3

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}


rm -rf tmp-install-cadical
mkdir -p tmp-install-cadical
pushd tmp-install-cadical

# latest on Github as of 18 November 2022
download https://github.com/arminbiere/cadical/archive/rel-$VERSION.tar.gz
tar xzf rel-$VERSION.tar.gz
cd cadical-rel-$VERSION
./configure
make -j${PROCESSES}
mkdir -p ${BIN_DIR}
cp build/cadical ${BIN_DIR}/cadical
echo "cadical executable is at ${BIN_DIR}/cadical"
ls -l ${BIN_DIR}/cadical
popd
rm -rf tmp-install-cadical
