#/bin/bash

set -o nounset
set -o errexit

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
    echo "ERROR: Give 1 solution file per param file in: $WD"
    exit 1
fi

if (( $# != 1 )); then
    echo "ERROR: Give a single parameter, mode to be used by conjure."
    echo "       Options: {df, best, random}"
    exit 1
fi

MODE=$1

SPEC=$(ls -1 *.essence | head -n 1)
SPEC=${SPEC%.essence}


rm -rf "$SPEC" $SPEC.errors
mkdir -p "$SPEC"


conjure --mode $MODE --in "$SPEC.essence" --out "$SPEC/$MODE.eprime" +RTS -s 2> conjure.stats

function perModelperParam {
    WD="$(pwd)"
    SPEC=$1
    MODEL=$2
    PARAM=$3

    conjure                                                                 \
        --mode       refineParam                                            \
        --in-essence $SPEC.essence                                          \
        --in-eprime  $MODEL.eprime                                          \
        --in-essence-param $PARAM.param                                     \
        --out-eprime-param $MODEL-$PARAM.eprime-param                       \
        +RTS -s 2> conjure_refineParam.stats

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
        --out-essence-solution  $MODEL-$PARAM.solution                      \
        +RTS -s 2> conjure_translateSolution.stats

    conjure                                                                 \
        --mode diff                                                         \
        $PARAM.solution                                                     \
        $MODEL-$PARAM.solution                                              \
        +RTS -s 2> conjure_diff.stats

    if (( $? != 0 )); then
        echo "$WD $MODEL $PARAM" >> fail.txt
    else
        echo "$WD $MODEL $PARAM" >> pass.txt
    fi
    echo "$WD $MODEL $PARAM" >> all.txt

}

export -f perModelperParam;

rm -f fail.txt pass.txt all.txt
touch fail.txt pass.txt all.txt

parallel -j1                                                                \
    perModelperParam "$SPEC" {1.} {2.}                                      \
        ::: $(ls -1 "$SPEC"/*.eprime)                                       \
        ::: $(ls -1 *.param)

