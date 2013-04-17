#!/bin/bash

set -o nounset

ESSENCE=$(ls *.essence | head -n 1)
ESSENCEBASE=${ESSENCE%.essence}

${CONJURE_REPO}/experiments/cp2013_model_selection/dbops/init.sh

(

cd "${ESSENCEBASE}-df"

ls *.eprime-minion-stats | \
parallel --tag cat {} 2>&1 | \
runhaskell ${CONJURE_REPO}/experiments/cp2013_model_selection/runExperiment_collectDataToDB.hs ${ESSENCEBASE} | \
sqlite3 ${CONJURE_REPO}/experiments/cp2013_model_selection/results.db

ls *.eprime-solution | \
parallel --tag cat {} 2>&1 | \
grep "minion Nodes\|minion TotalTime\|Savile Row TotalTime" | \
sed 's/\$ //g' | \
runhaskell ${CONJURE_REPO}/experiments/cp2013_model_selection/runExperiment_collectDataToDB.hs ${ESSENCEBASE} | \
sqlite3 ${CONJURE_REPO}/experiments/cp2013_model_selection/results.db

)

