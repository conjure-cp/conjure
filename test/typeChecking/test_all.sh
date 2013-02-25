#/bin/bash

set -o nounset
set -o errexit

export ROOT_DIR="$(pwd)"
export FAIL_FILE="$ROOT_DIR/fail.txt"
export PASS_FILE="$ROOT_DIR/pass.txt"

export FAIL_COUNT_FILE="$ROOT_DIR/countFail.txt"
export PASS_COUNT_FILE="$ROOT_DIR/countPass.txt"
export ALL_COUNT_FILE="$ROOT_DIR/countAll.txt"

function perEssence {
    WD="$(pwd)"
    SPEC=$1

    FLAG1=0
    FLAG2=0
    FLAG3=0

    conjure                                                                 \
        --mode  typeCheck                                                   \
        --in    "$SPEC.essence"                                             \
    FLAG1=$?

    conjure                                                                 \
        --mode  pretty                                                      \
        --in    "$SPEC.essence"                                             \
        --out   "$SPEC.pretty"
    FLAG2=$?

    conjure                                                                 \
        --mode  typeCheck                                                   \
        --in    "$SPEC.pretty"                                              \
    FLAG3=$?

    rm -f "$SPEC.pretty"

    if (( $FLAG1 != 0 )) ; then
        echo "[flag1] $PWD/$SPEC.essence" >> "$FAIL_FILE"
    else
        echo "[flag1] $PWD/$SPEC.essence" >> "$PASS_FILE"
    fi

    if (( $FLAG2 != 0 )) ; then
        echo "[flag2] $PWD/$PSEC.essence" >> "$FAIL_FILE"
    else
        echo "[flag2] $PWD/$PSEC.essence" >> "$PASS_FILE"
    fi

    if (( $FLAG3 != 0 )) ; then
        echo "[flag3] $PWD/$PSEC.essence" >> "$FAIL_FILE"
    else
        echo "[flag3] $PWD/$PSEC.essence" >> "$PASS_FILE"
    fi
}

export -f perEssence;

rm -f "$FAIL_FILE" "$PASS_FILE"
touch "$FAIL_FILE" "$PASS_FILE"

parallel perEssence {1.} ::: $(find . -name "*.essence")

FAIL_COUNT=$(cat "$FAIL_FILE" | wc -l | tr -d ' ')
PASS_COUNT=$(cat "$PASS_FILE" | wc -l | tr -d ' ')
ALL_COUNT=$(( $FAIL_COUNT + $PASS_COUNT ))
echo "YVALUE=$FAIL_COUNT" > "$FAIL_COUNT_FILE"
echo "YVALUE=$PASS_COUNT" > "$PASS_COUNT_FILE"
echo "YVALUE=$ALL_COUNT"  > "$ALL_COUNT_FILE"

echo "(Type checking) Number of failing tests: "
cat "$FAIL_COUNT_FILE"
cat "$FAIL_FILE"

echo "(Type checking) Number of passing tests: "
cat "$PASS_COUNT_FILE"

echo "(Type checking) Number of all tests: "
cat "$ALL_COUNT_FILE"

