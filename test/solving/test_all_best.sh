#/bin/bash

set -o nounset
set -o errexit

export SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
export SCRIPT_TEST_SINGLE="$SCRIPT_DIR/test_single.sh"

export FAIL_FILE="$SCRIPT_DIR/best_fail.txt"
export FAIL_COUNT_FILE="$SCRIPT_DIR/best_countFail.txt"

export PASS_FILE="$SCRIPT_DIR/best_pass.txt"
export PASS_COUNT_FILE="$SCRIPT_DIR/best_countPass.txt"

export ALL_FILE="$SCRIPT_DIR/best_all.txt"
export ALL_COUNT_FILE="$SCRIPT_DIR/best_countAll.txt"

export FILES="$FAIL_FILE $PASS_FILE $ALL_FILE $FAIL_COUNT_FILE $PASS_COUNT_FILE $ALL_COUNT_FILE"

rm -f $FILES
touch $FILES

function perDirectory {
    DIR="$1"

    cd "$DIR"
    echo "calling on $DIR"
    rm -f fail.txt pass.txt all.txt
    touch fail.txt pass.txt all.txt
    time bash "$SCRIPT_TEST_SINGLE" best
    cat fail.txt >> "$FAIL_FILE"
    cat pass.txt >> "$PASS_FILE"
    cat all.txt  >> "$ALL_FILE"
}

export -f perDirectory

find "$SCRIPT_DIR" -name "*.essence" | parallel perDirectory {//}

cat "$FAIL_FILE" | wc -l > "$FAIL_COUNT_FILE"
cat "$PASS_FILE" | wc -l > "$PASS_COUNT_FILE"
cat "$ALL_FILE"  | wc -l > "$ALL_COUNT_FILE"

echo -n "(best) Number of failing tests: "
cat "$FAIL_COUNT_FILE"
cat "$FAIL_FILE"

echo -n "(best) Number of passing tests: "
cat "$PASS_FILE"

echo -n "(best) Number of all tests: "
cat "$ALL_FILE"

