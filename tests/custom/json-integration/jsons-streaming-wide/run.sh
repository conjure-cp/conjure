#!/bin/bash

set -o errexit
set -o nounset

rm -rf conjure-output *.solution *.solutions* *.stats.json

# (1) several solutions in one file: one solution per line, each line valid JSON
conjure solve jsons.essence --copy-solutions=no --number-of-solutions=3 \
    --solutions-in-one-file --output-format=jsonstream >/dev/null
f=conjure-output/model000001.solutions.json
echo "one-file: lines=$(grep -c . $f) solutions=$(grep -c '"x"' $f)"
python3 -c "
import json
for l in open('$f'):
    if l.strip(): json.loads(l)
print('one-file: every line parses as JSON')"
rm -rf conjure-output

# (2) one file per solution: the file holds JSON, not a Haskell value
conjure solve jsons.essence --copy-solutions=no --number-of-solutions=1 \
    --output-format=jsonstream >/dev/null
g=conjure-output/model000001-solution000001.solution.json
echo "per-file: lines=$(grep -c . $g)"
python3 -c "
import json
json.load(open('$g'))
print('per-file: parses as JSON')"

rm -rf conjure-output *.solution *.solutions* *.stats.json
