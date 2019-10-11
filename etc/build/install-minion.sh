#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ${BIN_DIR}/tmp-install-minion
mkdir -p ${BIN_DIR}/tmp-install-minion
pushd ${BIN_DIR}/tmp-install-minion

function download {
    if which curl 2> /dev/null > /dev/null; then
        curl -L -O $1
    elif which wget 2> /dev/null > /dev/null; then
        wget --no-check-certificate -c $1
    else
        echo "You seem to have neither curl nor wget on this computer."
        echo "Cannot download without one of them."
        exit 1
    fi
}
export -f download

OS=$(uname)

if [ "$OS" == "Darwin" ]; then
    download https://savilerow.cs.st-andrews.ac.uk/savilerow-1.7.0RC-mac.tgz
    tar zxf savilerow-1.7.0RC-mac.tgz
    mv savilerow-1.7.0RC-mac/bin/minion ${BIN_DIR}/minion
elif [ "$OS" == "Linux" ]; then
    download https://savilerow.cs.st-andrews.ac.uk/savilerow-1.7.0RC-linux.tgz
    tar zxf savilerow-1.7.0RC-linux.tgz
    mv savilerow-1.7.0RC-linux/bin/minion ${BIN_DIR}/minion
else
    # assuming this is Windows
    echo "Your OS is (according to uname): ${OS}"
    download https://savilerow.cs.st-andrews.ac.uk/savilerow-1.6.5-windows.zip
    unzip savilerow-1.6.5-windows.zip
    mv savilerow-1.6.5-windows/bin/minion.exe ${BIN_DIR}/minion.exe
fi

echo "minion executable is at ${BIN_DIR}/minion"
ls -l ${BIN_DIR}/minion*
popd
rm -rf ${BIN_DIR}/tmp-install-minion

