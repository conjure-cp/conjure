#/bin/bash

set -o nounset

parallel bash runExperiment_conjure.sh ::: $(find -name "*.essence")

find . -name "*.essence" | parallel bash runExperiment_conjure.sh {}

