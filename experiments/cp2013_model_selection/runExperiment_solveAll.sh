#!/bin/bash

set -o nounset

ESSENCE=$(ls *.essence | head -n 1)
ESSENCEBASE=${ESSENCE%.essence}

parallel ${CONJURE_REPO}/experiments/cp2013_model_selection/runExperiment_perModelPerParam.sh ${ESSENCE} {1} {2} \
    ::: $(ls ${ESSENCEBASE}-df/*.eprime) \
    ::: $(ls *.param)

