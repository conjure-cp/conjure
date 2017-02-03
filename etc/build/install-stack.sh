#/bin/bash

set -o errexit
set -o nounset

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source ${SCRIPT_DIR}/default_envvars.sh

if ! which stack ; then
    echo "Installing Haskell build tool stack"
    if which curl; then
        curl -sSL https://get.haskellstack.org/ | sh
    elif which wget; then
        wget -qO- https://get.haskellstack.org/ | sh
    else
        echo "You seem to have neither curl nor wget on this computer."
        echo "Cannot install stack without one of them."
        exit 1
    fi
fi
