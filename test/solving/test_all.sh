#/bin/bash

set -o nounset


if (( $# < 3 )); then
    echo "ERROR:"
    echo "    - Give a directory path."
    echo "    - a build_frequency value."
    echo "        one of: {all, continuous, weekly}"
    echo "        \"all\" would run all tests, independent of their build_frequency values."
    echo "    - and a list of arguments, each a mode to be used by Conjure."
    echo "        Modes: {df, smallest, random}"
    exit 1
fi

export WD="$1"
export BUILD_FREQ="$2"
export MODES="${*:3}"

export SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
export SCRIPT_TEST_SINGLE="$SCRIPT_DIR/test_single.sh"


function perDirectory {
    MODE="$1"
    DIR="$2"

    if [ "$BUILD_FREQ" == "all" ] || [ "$BUILD_FREQ" == "$(cat ${DIR}/build_frequency 2> /dev/null)" ] ; then

    FAIL_FILE="$WD/${MODE}_fail.txt"
    PASS_FILE="$WD/${MODE}_pass.txt"
    ALL_FILE="$WD/${MODE}_all.txt"

    cd "$DIR"
    rm -f "${MODE}_fail.txt" "${MODE}_pass.txt"
    touch "${MODE}_fail.txt" "${MODE}_pass.txt"
    time bash "$SCRIPT_TEST_SINGLE" "$MODE"
    cat "${MODE}_fail.txt" >> "$FAIL_FILE"
    cat "${MODE}_pass.txt" >> "$PASS_FILE"
    cat "${MODE}_fail.txt" >> "$ALL_FILE"
    cat "${MODE}_pass.txt" >> "$ALL_FILE"

    fi
}

export -f perDirectory


for MODE in $MODES ; do
    FAIL_FILE="$WD/${MODE}_fail.txt"
    PASS_FILE="$WD/${MODE}_pass.txt"
    ALL_FILE="$WD/${MODE}_all.txt"

    FAIL_COUNT_FILE="$WD/${MODE}_countFail.txt"
    PASS_COUNT_FILE="$WD/${MODE}_countPass.txt"
    ALL_COUNT_FILE="$WD/${MODE}_countAll.txt"

    rm -f "$FAIL_FILE" "$PASS_FILE" "$ALL_FILE"
    touch "$FAIL_FILE" "$PASS_FILE" "$ALL_FILE"
done

parallel -k --tag perDirectory {1} {2//} ::: $MODES ::: $(find "$WD" -name "*.essence")

for MODE in $MODES ; do
    FAIL_FILE="$WD/${MODE}_fail.txt"
    PASS_FILE="$WD/${MODE}_pass.txt"
    ALL_FILE="$WD/${MODE}_all.txt"

    FAIL_COUNT_FILE="$WD/${MODE}_countFail.txt"
    PASS_COUNT_FILE="$WD/${MODE}_countPass.txt"
    ALL_COUNT_FILE="$WD/${MODE}_countAll.txt"

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

