#!/bin/bash

set -o errexit
set -o nounset

OS=$(uname)

if [ "$OS" == "Darwin" ]; then
    brew install minizinc sbt
elif [ "$OS" == "Linux" ]; then
    echo "This script doesn't know how to install minizinc and sbt on linux at the moment."
else
    echo "Cannot determine your OS, uname reports: ${OS}"
    exit 1
fi


export BIN_DIR=${BIN_DIR:-${HOME}/.cabal/bin}

rm -rf ~/tmp-install-oscar-cbls
mkdir ~/tmp-install-oscar-cbls
pushd ~/tmp-install-oscar-cbls
hg clone ssh://hg@bitbucket.org/oscarlib/oscar-releases
cd oscar-releases/oscar-cbls-flatzinc
bash setup.sh
cp fzn-oscar-cbls ${BIN_DIR}/fzn-oscar-cbls
cp mzn-oscar-cbls ${BIN_DIR}/mzn-oscar-cbls
cp -r mznlib-cbls ${BIN_DIR}/mznlib-cbls
ls -l ${BIN_DIR}/*cbls
popd
rm -rf ~/tmp-install-oscar-cbls

