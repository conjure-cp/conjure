#!/bin/bash

export LIMIT_TIME=${LIMIT_TIME:-0}

rm -f test_commands ; touch test_commands

echo "conjure/parse_print"      >> test_commands
echo "conjure/domainSize"       >> test_commands
echo "conjure/representations"  >> test_commands
echo "conjure/type-checking"    >> test_commands

echo "Using conjure-testing executable from the following location."
ls .stack-work/dist/*/*/build/conjure-testing/conjure-testing
cp .stack-work/dist/*/*/build/conjure-testing/conjure-testing .

./conjure-testing --limit-time ${LIMIT_TIME} -l > all-tests-list
cat all-tests-list | grep 'conjure/custom'     >> test_commands
cat all-tests-list | grep 'conjure/exhaustive' >> test_commands
rm all-tests-list

function t {
    ./conjure-testing --limit-time 0 -p "$1"
}
export -f t

parallel \
    --results test_results \
    --joblog test_joblog \
    --no-notice \
    --eta \
    "t {}" \
    :::: test_commands

grep FAIL -R test_results
echo "Number of failing tests: $(grep FAIL -R test_results | wc -l)"

