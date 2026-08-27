#!/bin/bash

set -o errexit
set -o nounset

rm -rf conjure-output *.solutions *.solutions.json *.stats.json

conjure modelling model.essence -o conjure-output -ax >/dev/null

# Every model must agree, and must agree on 45. Printing the distinct counts rather than
# one line per model keeps this stable if conjure produces a different number of models.
for eprime in conjure-output/*.eprime; do
    m=$(basename "$eprime" .eprime)
    conjure solve model.essence --use-existing-models="$m".eprime -o conjure-output \
        --number-of-solutions=all --solutions-in-one-file --copy-solutions=no \
        --output-format=jsonstream --line-width=100000 >/dev/null
    grep -c '"a"' conjure-output/"$m".solutions.json
done | sort -u

rm -rf conjure-output *.solutions *.solutions.json *.stats.json
