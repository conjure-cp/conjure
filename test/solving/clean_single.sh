#/bin/bash

# This scripts is used to clean the results of a single test run.
# It is the undo of test_single.sh
# Call it in the directory where you store the test case, and
# after running test_single.sh to delete all the generated files.

rm -f \
    *_fail.txt *_pass.txt   \
    *conjure.stats          \
    */*.eprime              \
    */*.eprime.logs         \
    */*.eprime-minion       \
    */*.eprime-minion-stats \
    */*.eprime-param        \
    */*.eprime-solution     \
    */*.solution

