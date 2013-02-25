#/bin/bash

set -o nounset
set -o errexit

export SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
export FAIL_FILE="$SCRIPT_DIR/fail.txt"
export PASS_FILE="$SCRIPT_DIR/pass.txt"

export FAIL_COUNT_FILE="$SCRIPT_DIR/countFail.txt"
export PASS_COUNT_FILE="$SCRIPT_DIR/countPass.txt"
export ALL_COUNT_FILE="$SCRIPT_DIR/countAll.txt"

function perEssence {
    SPEC=$1

    FLAG1=0
    FLAG2=0
    FLAG3=0

    conjure                                                                 \
        --mode  typeCheck                                                   \
        --in    "$SPEC.essence"                                             \
    FLAG1=$?

    if (( $FLAG1 != 0 )) ; then             # type-checking failed, stop.
        FLAG2=1
        FLAG3=1
    else                                    # type-checks, try pretty
        conjure                                                             \
            --mode  pretty                                                  \
            --in    "$SPEC.essence"                                         \
            --out   "$SPEC.pretty"
        FLAG2=$?
        if (( $FLAG2 != 0 )) ; then         # pretty failed, stop.
            FLAG3=1
        else                                # pretty worked, try type-checking prettified
            conjure                                                         \
                --mode  typeCheck                                           \
                --in    "$SPEC.pretty"                                      \
            FLAG3=$?
        fi
    fi

    rm -f "$SPEC.pretty"

    if (( $FLAG1 != 0 )) ; then
        echo "[flag1] $SPEC.essence" >> "$FAIL_FILE"
    else
        echo "[flag1] $SPEC.essence" >> "$PASS_FILE"
    fi

    if (( $FLAG2 != 0 )) ; then
        echo "[flag2] $SPEC.essence" >> "$FAIL_FILE"
    else
        echo "[flag2] $SPEC.essence" >> "$PASS_FILE"
    fi

    if (( $FLAG3 != 0 )) ; then
        echo "[flag3] $SPEC.essence" >> "$FAIL_FILE"
    else
        echo "[flag3] $SPEC.essence" >> "$PASS_FILE"
    fi
}

export -f perEssence;

rm -f "$FAIL_FILE" "$PASS_FILE"
touch "$FAIL_FILE" "$PASS_FILE"

parallel perEssence {1.} ::: $(find "$SCRIPT_DIR" -name "*.essence")

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

