#!/bin/bash

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf tmp-install-cadical
mkdir -p tmp-install-cadical
pushd tmp-install-cadical

download https://github.com/arminbiere/cadical/archive/rel-1.3.0.tar.gz
tar xzf rel-1.3.0.tar.gz
cd cadical-rel-1.3.0
./configure
make -j
mkdir -p ${BIN_DIR}
cp build/cadical ${BIN_DIR}/cadical
echo "cadical executable is at ${BIN_DIR}/cadical"
ls -l ${BIN_DIR}/cadical
popd
rm -rf tmp-install-cadical

