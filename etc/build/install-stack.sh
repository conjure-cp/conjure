#!/bin/bash

set -o errexit
set -o nounset

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export CI=${CI:-false}

function dlStack {
    if [ `uname` = "Darwin" ] ; then
        curl --insecure -L https://www.stackage.org/stack/osx-x86_64 | tar xz --strip-components=1 --include '*/stack' -C ${BIN_DIR}
    elif [ `uname` = "Linux" ] ; then
        curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ${BIN_DIR} '*/stack'
    else
        return 1
    fi
}
export -f dlStack


if ! which stack 2> /dev/null > /dev/null; then
    echo "Installing Haskell build tool stack to ${BIN_DIR}"
    if $CI; then
        dlStack
    else
        if dlStack ; then
            echo "Downloaded stack."
        else
            echo "Couldn't download stack, attempting build from source."
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
fi

ls -l $(which stack)
stack --version
