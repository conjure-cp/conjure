#/bin/bash

set -o nounset
set -o errexit

if (( $# != 1 )); then
    echo "ERROR: Give a single parameter, mode to be used by Conjure."
    echo "       Options: {df, best, random}"
    exit 1
fi

export MODE=$1

export SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
export SCRIPT_TEST_SINGLE="$SCRIPT_DIR/test_single.sh"

export FAIL_FILE="$SCRIPT_DIR/${MODE}_fail.txt"
export FAIL_COUNT_FILE="$SCRIPT_DIR/${MODE}_countFail.txt"

export PASS_FILE="$SCRIPT_DIR/${MODE}_pass.txt"
export PASS_COUNT_FILE="$SCRIPT_DIR/${MODE}_countPass.txt"

export ALL_FILE="$SCRIPT_DIR/${MODE}_all.txt"
export ALL_COUNT_FILE="$SCRIPT_DIR/${MODE}_countAll.txt"

export FILES="$FAIL_FILE $PASS_FILE $ALL_FILE $FAIL_COUNT_FILE $PASS_COUNT_FILE $ALL_COUNT_FILE"

rm -f $FILES
touch $FILES

function perDirectory {
    DIR="$1"

    cd "$DIR"
    echo "calling on $DIR"
    rm -f fail.txt pass.txt
    touch fail.txt pass.txt
    time bash "$SCRIPT_TEST_SINGLE" "$MODE"
    cat fail.txt >> "$FAIL_FILE"
    cat pass.txt >> "$PASS_FILE"
    cat fail.txt >> "$ALL_FILE"
    cat pass.txt >> "$ALL_FILE"
}

export -f perDirectory

find "$SCRIPT_DIR" -name "*.essence" | parallel --tag perDirectory "{//}"

FAIL_COUNT=$(cat "$FAIL_FILE" | wc -l | tr -d ' ')
PASS_COUNT=$(cat "$PASS_FILE" | wc -l | tr -d ' ')
ALL_COUNT=$( cat "$ALL_FILE"  | wc -l | tr -d ' ')
echo "YVALUE=$FAIL_COUNT" > "$FAIL_COUNT_FILE"
echo "YVALUE=$PASS_COUNT" > "$PASS_COUNT_FILE"
echo "YVALUE=$ALL_COUNT"  > "$ALL_COUNT_FILE"

echo "($MODE) Number of failing tests: "
cat "$FAIL_COUNT_FILE"
cat "$FAIL_FILE"

echo "($MODE) Number of passing tests: "
cat "$PASS_COUNT_FILE"

echo "($MODE) Number of all tests: "
cat "$ALL_COUNT_FILE"

