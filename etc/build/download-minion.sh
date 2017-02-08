#/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.cabal/bin}

wget -c https://dl.dropboxusercontent.com/u/14272760/SavileRow/minion-1.8-linux.tar_1.gz
tar -xvzf minion-1.8-linux.tar_1.gz
mv minion-1.8/bin/minion ${BIN_DIR}/minion

