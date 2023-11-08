#!/bin/bash

# version as of 18 November 2022
# we extract minion out of an SR release, so this is the version of the SR release
VERSION=1.9.1

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf tmp-install-minion
mkdir -p tmp-install-minion
pushd tmp-install-minion

OS=$(uname)

if [ "$OS" == "Darwin" ]; then
    download https://savilerow.cs.st-andrews.ac.uk/savilerow-$VERSION-mac.tgz
    tar zxf savilerow-$VERSION-mac.tgz
    mv savilerow-$VERSION-mac/bin/minion ${BIN_DIR}/minion
elif [ "$OS" == "Linux" ]; then
    download https://savilerow.cs.st-andrews.ac.uk/savilerow-$VERSION-linux.tgz
    tar zxf savilerow-$VERSION-linux.tgz
    mv savilerow-$VERSION-linux/bin/minion ${BIN_DIR}/minion
else
    # assuming this is Windows
    echo "Your OS is (according to uname): ${OS}"
    echo "No Minion for you."
fi

echo "minion executable is at ${BIN_DIR}/minion"
ls -l ${BIN_DIR}/minion*
popd
rm -rf tmp-install-minion

