#!/bin/bash

set -o errexit
set -o nounset

brew install minizinc sbt

export BIN_DIR=${BIN_DIR:-${HOME}/.cabal/bin}

rm -rf ~/tmp-install-oscar-cbls
mkdir ~/tmp-install-oscar-cbls
pushd ~/tmp-install-oscar-cbls
hg clone ssh://hg@bitbucket.org/oscarlib/oscar-releases
cd oscar-releases/oscar-cbls-flatzinc
bash setup.sh
cp fzn-oscar-cbls ${BIN_DIR}/fzn-oscar-cbls
cp mzn-oscar-cbls ${BIN_DIR}/mzn-oscar-cbls
ls -l ${BIN_DIR}/*oscar-cbls
popd
rm -rf ~/tmp-install-oscar-cbls

