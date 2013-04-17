#!/bin/bash

set -o nounset

ESSENCE=$(ls *.essence | head -n 1)
ESSENCEBASE=${ESSENCE%.essence}

(

cd "${ESSENCEBASE}-df"

ls *.eprime-minion-stats | \
parallel --tag cat {} 2>&1 | \
runhaskell ${CONJURE_REPO}/experiments/cp2013_model_selection/runExperiment_collectDataToDB.hs ${ESSENCEBASE} | \
parallel -j1

ls *.eprime-solution | \
parallel --tag cat {} 2>&1 | \
grep "minion Nodes\|minion TotalTime\|Savile Row TotalTime" | \
sed 's/\$ //g' | \
runhaskell ${CONJURE_REPO}/experiments/cp2013_model_selection/runExperiment_collectDataToDB.hs ${ESSENCEBASE} | \
parallel -j1

)

