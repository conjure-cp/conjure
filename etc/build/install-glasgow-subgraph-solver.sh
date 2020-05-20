#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}

rm -rf ${BIN_DIR}/tmp-install-glasgow
mkdir ${BIN_DIR}/tmp-install-glasgow
pushd ${BIN_DIR}/tmp-install-glasgow
git clone git@github.com:ciaranm/glasgow-subgraph-solver.git
cd glasgow-subgraph-solver
make
cp glasgow_clique_solver glasgow_common_subgraph_solver glasgow_subgraph_solver ${BIN_DIR}
ls -l ${BIN_DIR}/glasgow_clique_solver ${BIN_DIR}/glasgow_common_subgraph_solver ${BIN_DIR}/glasgow_subgraph_solver
popd
rm -rf ${BIN_DIR}/tmp-install-glasgow
