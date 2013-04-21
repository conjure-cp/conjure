#/bin/bash

set -o errexit
set -o nounset

DIR="$( cd "$( dirname "$0" )" && pwd )"
DIR="$DIR/../.."

OS=$(uname)

if [ "$OS" == "Darwin" ]; then
    ${DIR}/install_mac.sh
elif [ "$OS" == "Linux" ]; then
    ${DIR}/install_linux64.sh
else
    echo "Cannot determine your OS, uname reports: ${OS}"
fi

