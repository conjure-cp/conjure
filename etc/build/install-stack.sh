#!/bin/bash

set -o errexit
set -o nounset

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source ${SCRIPT_DIR}/default_envvars.sh

export BIN_DIR=${BIN_DIR:-${HOME}/.cabal/bin}
export CI=${CI:-false}


if ! which stack 2> /dev/null > /dev/null; then
    echo "Installing Haskell build tool stack to ${BIN_DIR}"
    if $CI; then
        if [ `uname` = "Darwin" ] ; then
            curl --insecure -L https://www.stackage.org/stack/osx-x86_64 | tar xz --strip-components=1 --include '*/stack' -C ~/.local/bin
        else
            curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
        fi
    else
        if which curl 2> /dev/null > /dev/null; then
            curl -sSL https://get.haskellstack.org/ | sh -s - -d ${BIN_DIR}
        elif which wget 2> /dev/null > /dev/null; then
            wget -qO- https://get.haskellstack.org/ | sh -s - -d ${BIN_DIR}
        else
            echo "You seem to have neither curl nor wget on this computer."
            echo "Cannot install stack without one of them."
            exit 1
        fi
    fi
fi

ls -l $(which stack)
stack --version
