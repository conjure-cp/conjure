#!/bin/bash

set -o errexit
set -o nounset

parallel tests/custom/acceptOutput.sh ::: $(find tests/custom -type d)
parallel tests/exhaustive/acceptOutput.sh ::: $(find tests/exhaustive -type d)
parallel tests/parse_print/acceptOutput.sh ::: $(find tests/parse_print -type d)
