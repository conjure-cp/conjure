#!/bin/bash

set -o errexit
set -o nounset

parallel -j4 tests/exhaustive/acceptOutput.sh ::: tests/exhaustive/*
parallel -j1 'hg revert {//} ; hg purge {//}' ::: $(find tests/exhaustive -name disabled.essence)

parallel -j4 tests/exhaustive/acceptOutput.sh ::: tests/parse_print/*
