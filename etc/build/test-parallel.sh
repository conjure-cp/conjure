#!/bin/bash

export LIMIT_TIME=${LIMIT_TIME:-0}

rm -f test_commands ; touch test_commands

echo "conjure/parse_print"      >> test_commands
echo "conjure/domainSize"       >> test_commands
echo "conjure/representations"  >> test_commands
echo "conjure/type-checking"    >> test_commands

dist/build/conjure-testing/conjure-testing --limit-time ${LIMIT_TIME} -l > all-tests-list
cat all-tests-list | grep 'conjure/custom'     >> test_commands
cat all-tests-list | grep 'conjure/exhaustive' >> test_commands
rm all-tests-list

parallel \
    --results test_results \
    --joblog test_joblog \
    --no-notice \
    --eta \
    "dist/build/conjure-testing/conjure-testing --limit-time 0 -p {}" \
    :::: test_commands

grep FAIL -R test_results
echo "Number of failing tests: $(grep FAIL -R test_results | wc -l)"

