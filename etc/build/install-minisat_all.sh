#!/bin/bash

# version as of 18 November 2022
VERSION=1.1.2

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}


rm -rf tmp-install-minisat_all
mkdir -p tmp-install-minisat_all
pushd tmp-install-minisat_all

git clone https://github.com/taboege/raku-SAT-Enumerator-Toda.git
cd raku-SAT-Enumerator-Toda
git checkout f1fa90f88fbf0a8c7f46804bfeeffa74effa5475
for solver in bc_minisat_all nbc_minisat_all bdd_minisat_all ; do
    pushd src/${solver}
    make -j${PROCESSES} ${solver}_release
    mv ${solver}_release ${BIN_DIR}/${solver}_release
    echo "${solver} executable is at ${BIN_DIR}/${solver}_release"
    ls -l ${BIN_DIR}/${solver}_release
    popd
done

echo "solver executables are at ${BIN_DIR}"
ls -l ${BIN_DIR}/*minisat_all_release

popd
rm -rf tmp-install-minisat_all
