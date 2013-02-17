#/bin/bash

SPEC=$(ls -1 *.essence | head -n 1)
SPEC=${SPEC%.essence}

rm -rf "$SPEC"
mkdir -p "$SPEC"

#conjure --mode df   --in "$SPEC.essence"
conjure --mode best --in "$SPEC.essence" --out "$SPEC/best.eprime"

function perModelperParam {

    SPEC=$1
    MODEL=$2
    PARAM=$3

    echo "$MODEL x $PARAM"

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

    conjure-solution                                                        \
        $MODEL.eprime                                                       \
        $MODEL-$PARAM.eprime-solution                                       \
        $SPEC.essence                                                       \
        $PARAM.param                                                        \
        $MODEL-$PARAM.eprime-param

}

export -f perModelperParam;

parallel -u -j1                                                             \
    perModelperParam "$SPEC" {1.} {2.}                                      \
        ::: $(ls -1 "$SPEC"/*.eprime)                                       \
        ::: $(ls -1 *.param)

