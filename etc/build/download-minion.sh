#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.cabal/bin}

OS=$(uname)

if [ "$OS" == "Darwin" ]; then
    wget -c https://ozgur.host.cs.st-andrews.ac.uk/Minions/minion-1.8-mac.tar_.gz
    tar -xvzf minion-1.8-mac.tar_.gz
    mv minion-1.8/bin/minion ${BIN_DIR}/minion
elif [ "$OS" == "Linux" ]; then
    wget -c https://ozgur.host.cs.st-andrews.ac.uk/Minions/minion-1.8-linux.tar_1.gz
    tar -xvzf minion-1.8-linux.tar_1.gz
    mv minion-1.8/bin/minion ${BIN_DIR}/minion
else
    echo "Cannot determine your OS, uname reports: ${OS}"
    exit 1
fi

