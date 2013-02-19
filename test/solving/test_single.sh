#/bin/bash

set -o nounset
set -o errexit

if (( $(ls -1 *.essence 2> /dev/null | wc -l) != 1 )); then
    echo "ERROR: Only 1 *.essence file should be in this directory."
    exit 1
fi

if (( $(ls -1 *.param 2> /dev/null | wc -l) == 0 )); then
    echo "ERROR: At least 1 *.param file should be in this directory."
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


conjure --mode $MODE --in "$SPEC.essence" --out "$SPEC/$MODE.eprime"

function perModelperParam {

    SPEC=$1
    MODEL=$2
    PARAM=$3

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

    conjure                                                                 \
        --mode diff                                                         \
        $PARAM.solution                                                     \
        $MODEL-$PARAM.solution

    if (( $? != 0 )); then
        echo "$SPEC $MODEL $PARAM" >> $SPEC.errors
    fi

}

export -f perModelperParam;

parallel -u -j1                                                             \
    perModelperParam "$SPEC" {1.} {2.}                                      \
        ::: $(ls -1 "$SPEC"/*.eprime)                                       \
        ::: $(ls -1 *.param)

