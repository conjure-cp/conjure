#/bin/bash

set -o nounset

ESSENCE=$(ls *.essence | head -n 1)

conjure --mode df --in "$ESSENCE" +RTS -M16G -s 2> >(tee "${ESSENCE}.conjure_stats" >&2)

