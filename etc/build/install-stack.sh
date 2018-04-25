#!/bin/bash

set -o errexit
set -o nounset

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source ${SCRIPT_DIR}/default_envvars.sh

if ! which stack 2> /dev/null > /dev/null; then
    echo "Installing Haskell build tool stack to ~/.local/bin"
    if which curl 2> /dev/null > /dev/null; then
        curl -sSL https://get.haskellstack.org/ | sh -s - -d ~/.local/bin
    elif which wget 2> /dev/null > /dev/null; then
        wget -qO- https://get.haskellstack.org/ | sh -s - -d ~/.local/bin
    else
        echo "You seem to have neither curl nor wget on this computer."
        echo "Cannot install stack without one of them."
        exit 1
    fi
fi
