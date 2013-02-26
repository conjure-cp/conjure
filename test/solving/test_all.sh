#/bin/bash

set -o nounset
set -o errexit

if (( $# == 0 )); then
    echo "ERROR: Give a list of arguments, each a mode to be used by Conjure."
    echo "       Options: {df, best, random}"
    exit 1
fi

export MODES="$@"

export SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
export SCRIPT_TEST_SINGLE="$SCRIPT_DIR/test_single.sh"


function perDirectory {
    MODE="$1"
    DIR="$2"

    FAIL_FILE="$SCRIPT_DIR/${MODE}_fail.txt"
    PASS_FILE="$SCRIPT_DIR/${MODE}_pass.txt"
    ALL_FILE="$SCRIPT_DIR/${MODE}_all.txt"

    cd "$DIR"
    rm -f fail.txt pass.txt
    touch fail.txt pass.txt
    time bash "$SCRIPT_TEST_SINGLE" "$MODE"
    cat fail.txt >> "$FAIL_FILE"
    cat pass.txt >> "$PASS_FILE"
    cat fail.txt >> "$ALL_FILE"
    cat pass.txt >> "$ALL_FILE"
}

export -f perDirectory


for MODE in $MODES ; do
    FAIL_FILE="$SCRIPT_DIR/${MODE}_fail.txt"
    PASS_FILE="$SCRIPT_DIR/${MODE}_pass.txt"
    ALL_FILE="$SCRIPT_DIR/${MODE}_all.txt"

    FAIL_COUNT_FILE="$SCRIPT_DIR/${MODE}_countFail.txt"
    PASS_COUNT_FILE="$SCRIPT_DIR/${MODE}_countPass.txt"
    ALL_COUNT_FILE="$SCRIPT_DIR/${MODE}_countAll.txt"

    rm -f "$FAIL_FILE" "$PASS_FILE" "$ALL_FILE"
done

parallel --tag perDirectory {1} {2//} ::: $MODES ::: $(find "$SCRIPT_DIR" -name "*.essence")

for MODE in $MODES ; do
    FAIL_FILE="$SCRIPT_DIR/${MODE}_fail.txt"
    PASS_FILE="$SCRIPT_DIR/${MODE}_pass.txt"
    ALL_FILE="$SCRIPT_DIR/${MODE}_all.txt"

    FAIL_COUNT_FILE="$SCRIPT_DIR/${MODE}_countFail.txt"
    PASS_COUNT_FILE="$SCRIPT_DIR/${MODE}_countPass.txt"
    ALL_COUNT_FILE="$SCRIPT_DIR/${MODE}_countAll.txt"

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
done

