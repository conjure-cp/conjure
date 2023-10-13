#!/bin/bash

set -o errexit
set -o nounset

parallel --no-notice tests/custom/acceptOutput.sh ::: $(find tests/custom -type d)
parallel --no-notice tests/exhaustive/acceptOutput.sh ::: $(find tests/exhaustive -type d)
parallel --no-notice tests/parse_print/acceptOutput.sh ::: $(find tests/parse_print -type d)