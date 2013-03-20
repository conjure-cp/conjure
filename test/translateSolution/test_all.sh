#!/bin/sh

set -o nounset

export SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
export FAIL_FILE="$SCRIPT_DIR/fail.txt"

export FAIL_COUNT_FILE="$SCRIPT_DIR/countFail.txt"
export PASS_COUNT_FILE="$SCRIPT_DIR/countPass.txt"
export ALL_COUNT_FILE="$SCRIPT_DIR/countAll.txt"

# run tests and unescape newlines
upTests --format=failed-examples --color=never | sed $'s/\\\\n/\\\n/g' > $FAIL_FILE

results="`tail $FAIL_FILE  | grep examples`"
ALL_COUNT="`echo "$results" | egrep -o '^[0-9]+'`"
FAIL_COUNT="`echo "$results" | egrep -o ', [0-9]+' | tr -d ', '`"
PASS_COUNT="$(($ALL_COUNT-$FAIL_COUNT))"

echo "YVALUE=$FAIL_COUNT" > "$FAIL_COUNT_FILE"
echo "YVALUE=$PASS_COUNT" > "$PASS_COUNT_FILE"
echo "YVALUE=$ALL_COUNT"  > "$ALL_COUNT_FILE"

echo "(Translate Solution) Number of failing tests: "
cat "$FAIL_COUNT_FILE"
cat $FAIL_FILE

echo "(Translate Solution) Number of passing tests: "
cat "$PASS_COUNT_FILE"

echo "(Translate Solution) Number of all tests: "
cat "$ALL_COUNT_FILE"
