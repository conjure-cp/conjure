#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export COMPILER=${COMPILER:-""}

rm -rf ~/tmp-install-minion
mkdir ~/tmp-install-minion
pushd ~/tmp-install-minion
hg clone https://bitbucket.org/stacs_cp/minion
mkdir -p minion/build
(
    cd minion/build
    if [ -z "${COMPILER}" ]; then
        ../build.py
    else
        if which ccache; then
            COMPILER="ccache ${COMPILER}"
        fi
        ../build.py --compiler "${COMPILER}"
    fi
    make minion
)
cp minion/build/minion ${BIN_DIR}/minion
echo "minion executable is at ${BIN_DIR}/minion"
ls -l ${BIN_DIR}/minion
popd
rm -rf ~/tmp-install-minion

