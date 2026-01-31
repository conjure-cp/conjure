#!/bin/bash

# version as of Dec 2025
VERSION=4.2.1

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

rm -rf tmp-install-glucose
mkdir tmp-install-glucose
pushd tmp-install-glucose
git clone https://github.com/audemard/glucose.git
cd glucose
git checkout $VERSION

################################################################################
# Glucose's Linux FPU control block is x86-only; same guard used in install-wmaxcdcl.sh.
python3 "${SCRIPT_DIR}/patch-x86-fpu-guard.py" simp/Main.cc parallel/Main.cc
################################################################################

(
    cd simp
    make -j${PROCESSES} r
    cp glucose_release ${BIN_DIR}/glucose
    echo "glucose executable is at ${BIN_DIR}/glucose"
    ls -l ${BIN_DIR}/glucose
)
(
    cd parallel
    make -j${PROCESSES} r
    cp glucose-syrup_release ${BIN_DIR}/glucose-syrup
    echo "glucose-syrup executable is at ${BIN_DIR}/glucose-syrup"
    ls -l ${BIN_DIR}/glucose-syrup
)
popd
rm -rf tmp-install-glucose
