#!/bin/bash

set -o errexit
set -o nounset

parallel -j4 tests/custom/acceptOutput.sh ::: $(find tests/custom -type d)
parallel -j4 tests/exhaustive/acceptOutput.sh ::: $(find tests/exhaustive -type d)
parallel -j4 tests/parse_print/acceptOutput.sh ::: $(find tests/parse_print -type d)
