#!/bin/bash

# WMaxCDCL (MSE 2023 solver source archive)
# version: as packaged by MaxSAT Evaluation 2023 (archive does not expose a semantic version)

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

LOCAL_WMAXCDCL_DIR="${REPO_ROOT}/WMaxCDCL"
LOCAL_WMAXCDCL_ZIP="${REPO_ROOT}/WMaxCDCL.zip"
URL="https://maxsat-evaluations.github.io/2023/mse23-solver-src/exact/WMaxCDCL.zip"

rm -rf tmp-install-wmaxcdcl
mkdir -p tmp-install-wmaxcdcl
pushd tmp-install-wmaxcdcl
download "${URL}" WMaxCDCL.zip
unzip WMaxCDCL.zip

################################################################################
# Same x86-only FPU guard fix as install-glucose.sh.
python3 "${SCRIPT_DIR}/patch-x86-fpu-guard.py" WMaxCDCL/code/simp/Main.cc
################################################################################

cd WMaxCDCL/code/simp
make -j${PROCESSES}
mkdir -p "${BIN_DIR}"
cp -f wmaxcdcl "${BIN_DIR}/wmaxcdcl"
echo "WMaxCDCL executable is at ${BIN_DIR}/wmaxcdcl"
ls -l "${BIN_DIR}/wmaxcdcl"
popd
rm -rf tmp-install-wmaxcdcl
