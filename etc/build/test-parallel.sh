#!/bin/bash

export LIMIT_TIME=${LIMIT_TIME:-0}

rm -f test_commands ; touch test_commands

echo "conjure.parse_print"      >> test_commands
echo "conjure.domainSize"       >> test_commands
echo "conjure.representations"  >> test_commands
echo "conjure.type-checking"    >> test_commands

echo "Using conjure-testing executable from the following location."
ls .stack-work/dist/*/*/build/conjure-testing/conjure-testing
cp .stack-work/dist/*/*/build/conjure-testing/conjure-testing .

# these are the 20 longest-running tests, let's put them at the front
if [ ${LIMIT_TIME} = 0 ]; then
    echo "conjure.exhaustive.tildeOrd.tildeOrd_partition_01"        >> test_commands
    echo "conjure.exhaustive.basic.partition_06"                    >> test_commands
    echo "conjure.exhaustive.autogen.gen14_1"                       >> test_commands
    echo "conjure.exhaustive.basic.partition_05_2"                  >> test_commands
    echo "conjure.exhaustive.basic.relation04_param"                >> test_commands
    echo "conjure.exhaustive.basic.partition_05_1"                  >> test_commands
    echo "conjure.exhaustive.autogen.gen14_2"                       >> test_commands
    echo "conjure.exhaustive.basic.set06"                           >> test_commands
    echo "conjure.exhaustive.basic.set09"                           >> test_commands
    echo "conjure.exhaustive.basic.typed01"                         >> test_commands
    echo "conjure.exhaustive.basic.function_range"                  >> test_commands
    echo "conjure.exhaustive.basic.setOfSet04"                      >> test_commands
    echo "conjure.exhaustive.issues.286"                            >> test_commands
    echo "conjure.exhaustive.basic.comprehension_letting"           >> test_commands
    echo "conjure.exhaustive.issues.166"                            >> test_commands
    echo "conjure.exhaustive.tildeOrd.tildeOrd_mset_01"             >> test_commands
    echo "conjure.exhaustive.basic.set05"                           >> test_commands
    echo "conjure.exhaustive.basic.set04"                           >> test_commands
    echo "conjure.exhaustive.basic.cut_01_on"                       >> test_commands
    echo "conjure.exhaustive.autogen.gen32"                         >> test_commands
fi

./conjure-testing --limit-time ${LIMIT_TIME} -l > all-tests-list
cat all-tests-list | grep 'conjure.custom'     >> test_commands
cat all-tests-list | grep 'conjure.exhaustive' >> test_commands
rm all-tests-list

# remove duplicates (see https://stackoverflow.com/a/11532197/463977)
awk '!x[$0]++' test_commands > test_commands_uniq
mv test_commands_uniq test_commands

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

