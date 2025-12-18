#!/bin/bash

# version as of Dec 2025
VERSION=v9.14

set -o errexit
set -o nounset
set -o pipefail

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export LIB_DIR=${LIB_DIR:-${HOME}/.local/lib}
export PROCESSES=${PROCESSES:-1}

mkdir -p ${BIN_DIR}
mkdir -p ${LIB_DIR}

BIN_DIR=$(cd "${BIN_DIR}" && pwd)
LIB_DIR=$(cd "${LIB_DIR}" && pwd)
BIN_PREFIX=$(dirname "${BIN_DIR}")
LIB_PREFIX=$(dirname "${LIB_DIR}")

if [[ "${BIN_PREFIX}" != "${LIB_PREFIX}" ]]; then
  echo "BIN_DIR and LIB_DIR must share the same parent so fzn-cp-sat can find the libraries at runtime." >&2
  echo "BIN_DIR=${BIN_DIR}" >&2
  echo "LIB_DIR=${LIB_DIR}" >&2
  exit 1
fi

rm -rf tmp-install-ortools
mkdir tmp-install-ortools
pushd tmp-install-ortools
git clone https://github.com/google/or-tools.git
cd or-tools
git checkout $VERSION
STAGING_PREFIX="$(pwd)/stage-install"
mkdir -p "${STAGING_PREFIX}"
cmake -S . -B build \
  -DBUILD_DEPS=ON \
  -DUSE_COINOR=OFF \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX="${STAGING_PREFIX}" \
  -DCMAKE_INSTALL_BINDIR=bin \
  -DCMAKE_INSTALL_LIBDIR=lib
cmake --build build --config Release --target install -j${PROCESSES}
cp "${STAGING_PREFIX}/bin/fzn-cp-sat" "${BIN_DIR}/fzn-cp-sat"
shopt -s nullglob
for lib_path in "${STAGING_PREFIX}/lib/"*; do
  cp -a "${lib_path}" "${LIB_DIR}/"
done
shopt -u nullglob
echo "ortools executable is at ${BIN_DIR}/fzn-cp-sat"
ls -l ${BIN_DIR}/fzn-cp-sat
popd
rm -rf tmp-install-ortools
