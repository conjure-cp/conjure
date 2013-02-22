#/bin/bash

set -o nounset
set -o errexit

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"

(
    cd "$SCRIPT_DIR"
    rm -f fail.txt pass.txt all.txt
    touch fail.txt pass.txt all.txt
)

function perDirectory {
    SCRIPT_DIR="$1"
    TEST_SINGLE="$2"
    DIR="$3"

    echo "calling on $DIR"
    ( cd "$DIR" ; time bash "$TEST_SINGLE" df ; touch fail.txt pass.txt all.txt )
    cat "$DIR/fail.txt" >> "$SCRIPT_DIR"/fail.txt
    cat "$DIR/pass.txt" >> "$SCRIPT_DIR"/pass.txt
    cat "$DIR/all.txt"  >> "$SCRIPT_DIR"/all.txt
}

export -f perDirectory

find "$SCRIPT_DIR" -name "*.essence" | parallel perDirectory "$SCRIPT_DIR" "$SCRIPT_DIR/test_single.sh" {//}

(
    cd "$SCRIPT_DIR"
    cat fail.txt | wc -l > countFail.txt
    cat pass.txt | wc -l > countPass.txt
    cat all.txt  | wc -l > countAll.txt
)

