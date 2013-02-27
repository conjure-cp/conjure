#/bin/bash

set -o nounset


COUNT_ESSENCE=$(ls -1 *.essence 2> /dev/null | wc -l)
COUNT_PARAM=$(ls -1 *.param 2> /dev/null | wc -l)
COUNT_SOLUTION=$(ls -1 *.solution 2> /dev/null | wc -l)

if (( $COUNT_ESSENCE != 1 )); then
    WD="$(pwd)"
    echo "ERROR: Only 1 *.essence file should be in: $WD"
    exit 1
fi

if (( $COUNT_PARAM == 0 )); then
    WD="$(pwd)"
    echo "ERROR: At least 1 *.param file should be in: $WD"
    exit 1
fi

if (( $COUNT_PARAM != $COUNT_SOLUTION )); then
    WD="$(pwd)"
    echo "Warning: You need to give 1 solution file per param file"
    echo "         if you want to compare against an expected solution."
    echo "         In: $WD"
fi

if (( $# != 1 )); then
    echo "ERROR: Give a single parameter, mode to be used by Conjure."
    echo "       Options: {df, smallest, random}"
    exit 1
fi

export WD="$(pwd)"

export MODE=$1
export FAIL_FILE="${MODE}_fail.txt"
export PASS_FILE="${MODE}_pass.txt"

export SPEC=$(ls -1 *.essence | head -n 1)
export SPEC=${SPEC%.essence}

export OUT_DIR="$SPEC-$MODE"
if [ $MODE == "df" ] ; then
    OUT_DIR="$SPEC"
fi

rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"



function perModelperParam {
    MODEL=$1
    PARAM=$2

    conjure                                                                 \
        --mode       refineParam                                            \
        --in-essence $SPEC.essence                                          \
        --in-eprime  $MODEL.eprime                                          \
        --in-essence-param $PARAM.param                                     \
        --out-eprime-param $MODEL-$PARAM.eprime-param

    savilerow                                                               \
        -in-eprime    $MODEL.eprime                                         \
        -in-param     $MODEL-$PARAM.eprime-param                            \
        -out-minion   $MODEL-$PARAM.eprime-minion                           \
        -out-solution $MODEL-$PARAM.eprime-solution

    conjure                                                                 \
        --mode translateSolution                                            \
        --in-essence            $SPEC.essence                               \
        --in-essence-param      $PARAM.param                                \
        --in-eprime             $MODEL.eprime                               \
        --in-eprime-param       $MODEL-$PARAM.eprime-param                  \
        --in-eprime-solution    $MODEL-$PARAM.eprime-solution               \
        --out-essence-solution  $MODEL-$PARAM.solution

    SOL_VALIDATE=0
    SOL_DIFF=0

    conjure                                                                 \
        --mode validateSolution                                             \
        --in-essence  $SPEC.essence                                         \
        --in-param    $PARAM.param                                          \
        --in-solution $MODEL-$PARAM.solution
    SOL_VALIDATE=$?

    if [ -f "$PARAM.solution" ] ; then
        conjure                                                             \
            --mode diff                                                     \
            $PARAM.solution                                                 \
            $MODEL-$PARAM.solution
        SOL_DIFF=$?
    fi

    if (( $SOL_VALIDATE != 0 )) ; then
        echo "[validateSolution] $WD $MODEL $PARAM" >> "$FAIL_FILE"
    else
        echo "[validateSolution] $WD $MODEL $PARAM" >> "$PASS_FILE"
    fi

    if (( $SOL_DIFF != 0 )) ; then
        echo "[  diffSolution  ] $WD $MODEL $PARAM" >> "$FAIL_FILE"
    else
        echo "[  diffSolution  ] $WD $MODEL $PARAM" >> "$PASS_FILE"
    fi

}

export -f perModelperParam;



rm -f "$FAIL_FILE" "$PASS_FILE"
touch "$FAIL_FILE" "$PASS_FILE"

conjure --mode $MODE --in "$SPEC.essence" --out "$OUT_DIR/$MODE.eprime" +RTS -M8G -s 2> >(tee "${MODE}_conjure.stats" >&2)

NB_EPRIMES=$(ls -1 "$OUT_DIR"/*.eprime 2> /dev/null | wc -l)

if (( $NB_EPRIMES == 0 )) ; then
    echo "[   no outputs   ] $WD" >> "$FAIL_FILE"
else
    parallel -j1                                                            \
        perModelperParam {1.} {2.}                                          \
            ::: $(ls -1 "$OUT_DIR"/*.eprime)                                \
            ::: $(ls -1 *.param)
fi

