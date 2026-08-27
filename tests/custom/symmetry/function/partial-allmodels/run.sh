#!/bin/bash

set -o errexit
set -o nounset

rm -rf conjure-output *.solutions *.solutions.json *.stats.json

conjure modelling model.essence -o conjure-output -ax \
    --unnamed-symmetry-breaking=Complete-AllPermutations-Independently >/dev/null

# Every model must agree, and must agree on 6 (one representative per orbit). Printing
# the distinct counts rather than one line per model deliberately avoids depending on
# how many models conjure generates, or on the order it generates them in.
for eprime in conjure-output/*.eprime; do
    m=$(basename "$eprime" .eprime)
    conjure solve model.essence --use-existing-models="$m".eprime -o conjure-output \
        --number-of-solutions=all --solutions-in-one-file --copy-solutions=no \
        --output-format=jsonstream --line-width=100000 >/dev/null
    grep -c '"x"' conjure-output/"$m".solutions.json
done | sort -u

rm -rf conjure-output *.solutions *.solutions.json *.stats.json
