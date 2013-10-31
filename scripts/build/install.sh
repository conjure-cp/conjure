#/bin/bash

set -o errexit
set -o nounset

DIR="$( cd "$( dirname "$0" )" && pwd )"

OS=$(uname)

if [ "$OS" == "Darwin" ]; then
    "${DIR}"/install_common.sh "apple-darwin"
elif [ "$OS" == "Linux" ]; then
    "${DIR}"/install_common.sh "unknown-linux"
else
    echo "Cannot determine your OS, uname reports: ${OS}"
fi

