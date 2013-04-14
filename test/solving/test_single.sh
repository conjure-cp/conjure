#/bin/bash

set -o nounset

echo "conjure repository is at ${CONJURE_REPO}"

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
    echo "Warning: No *.param file found, using an empty one."
    cp "${CONJURE_REPO}/files/empty.param" .
    COUNT_PARAM=1
fi

if (( $COUNT_PARAM != $COUNT_SOLUTION )); then
    WD="$(pwd)"
    echo "Warning: You need to give 1 solution file per param file"
    echo "         if you want to compare against an expected solution."
    echo "         In: $WD"
fi

if (( $# != 1 )); then
    echo "ERROR: Give a single parameter, mode to be used by Conjure."
    echo "       Options: {df, compact, random, first}"
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
    OUT_DIR="${SPEC}-df"
elif [ $MODE == "df-no-channelling" ] ; then
    OUT_DIR="${SPEC}-df-no-channelling"
fi


function perModelperParam {
    MODEL=$1
    PARAM=$2

    MSG_TEMPLATE="$MODE $WD $MODEL $PARAM"


    RESULTOF_REFINEPARAM=0
    MSG_REFINEPARAM="[refineParam] $MSG_TEMPLATE"
    echo "$MSG_REFINEPARAM"
    # echo "conjure --mode refineParam --in-essence $SPEC.essence --in-eprime $MODEL.eprime --in-essence-param $PARAM.param --out-eprime-param $MODEL-$PARAM.eprime-param"
    conjure                                                                 \
        --mode       refineParam                                            \
        --in-essence $SPEC.essence                                          \
        --in-eprime  $MODEL.eprime                                          \
        --in-essence-param $PARAM.param                                     \
        --out-eprime-param $MODEL-$PARAM.eprime-param
    RESULTOF_REFINEPARAM=$?
    if (( $RESULTOF_REFINEPARAM != 0 )) ; then
        echo "$MSG_REFINEPARAM" >> "$FAIL_FILE"
        exit 1
    fi


    RESULTOF_SAVILEROW=0
    MSG_SAVILEROW="[savilerow] $MSG_TEMPLATE"
    echo "$MSG_SAVILEROW"
    # echo "savilerow -in-eprime $MODEL.eprime -in-param $MODEL-$PARAM.eprime-param -out-minion $MODEL-$PARAM.eprime-minion -out-solution $MODEL-$PARAM.eprime-solution"
    savilerow                                                               \
        -in-eprime    $MODEL.eprime                                         \
        -in-param     $MODEL-$PARAM.eprime-param                            \
        -out-minion   $MODEL-$PARAM.eprime-minion                           \
        -out-solution $MODEL-$PARAM.eprime-solution                         \
        -minion-options "-timelimit 300"
    RESULTOF_SAVILEROW=$?
    if (( $RESULTOF_SAVILEROW != 0 )) ; then
        echo "$MSG_SAVILEROW" >> "$FAIL_FILE"
        exit 1
    fi


    RESULTOF_TRANSLATESOLN=0
    MSG_TRANSLATESOLN="[translateSolution] $MSG_TEMPLATE"
    echo "$MSG_TRANSLATESOLN"
    # echo "conjure --mode translateSolution --in-essence $SPEC.essence --in-essence-param $PARAM.param --in-eprime $MODEL.eprime --in-eprime-param $MODEL-$PARAM.eprime-param --in-eprime-solution $MODEL-$PARAM.eprime-solution --out-essence-solution $MODEL-$PARAM.solution"
    conjure                                                                 \
        --mode translateSolution                                            \
        --in-essence            $SPEC.essence                               \
        --in-essence-param      $PARAM.param                                \
        --in-eprime             $MODEL.eprime                               \
        --in-eprime-param       $MODEL-$PARAM.eprime-param                  \
        --in-eprime-solution    $MODEL-$PARAM.eprime-solution               \
        --out-essence-solution  $MODEL-$PARAM.solution
    RESULTOF_TRANSLATESOLN=$?
    if (( $RESULTOF_TRANSLATESOLN != 0 )) ; then
        echo "$MSG_TRANSLATESOLN" >> "$FAIL_FILE"
        exit 1
    fi


    RESULTOF_VALIDATESOLN=0
    MSG_VALIDATESOLN="[validateSolution] $MSG_TEMPLATE"
    echo "$MSG_VALIDATESOLN"
    # echo "conjure --mode validateSolution --in-essence $SPEC.essence --in-param $PARAM.param --in-solution $MODEL-$PARAM.solution"
    conjure                                                                 \
        --mode validateSolution                                             \
        --in-essence  $SPEC.essence                                         \
        --in-param    $PARAM.param                                          \
        --in-solution $MODEL-$PARAM.solution
    RESULTOF_VALIDATESOLN=$?
    if (( $RESULTOF_VALIDATESOLN != 0 )) ; then
        echo "$MSG_VALIDATESOLN" >> "$FAIL_FILE"
    else
        echo "$MSG_VALIDATESOLN" >> "$PASS_FILE"
    fi


    if [ -f "$PARAM.solution" ] ; then
        RESULTOF_DIFF=0
        MSG_DIFF="[diffSolution] $MSG_TEMPLATE"
        echo "$MSG_DIFF"
        # echo "conjure --mode diff $PARAM.solution $MODEL-$PARAM.solution"
        conjure                                                             \
            --mode diff                                                     \
            $PARAM.solution                                                 \
            $MODEL-$PARAM.solution
        RESULTOF_DIFF=$?
        if (( $RESULTOF_DIFF != 0 )) ; then
            echo "$MSG_DIFF" >> "$FAIL_FILE"
        else
            echo "$MSG_DIFF" >> "$PASS_FILE"
        fi
    fi


}

export -f perModelperParam;


rm -f "$FAIL_FILE" "$PASS_FILE"
touch "$FAIL_FILE" "$PASS_FILE"

rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"
conjure --mode $MODE --in "$SPEC.essence" --out "$OUT_DIR/$MODE.eprime" +RTS -M16G -s 2> >(tee "${MODE}_conjure.stats" >&2)

NB_EPRIMES=$(ls -1 "$OUT_DIR"/*.eprime 2> /dev/null | wc -l)

if (( $NB_EPRIMES == 0 )) ; then
    echo "[generatesZeroModels] $MODE $WD" >> "$FAIL_FILE"
else
    parallel -j3                                                            \
        perModelperParam {1.} {2.}                                          \
            ::: $(ls -1 "$OUT_DIR"/*.eprime)                                \
            ::: $(ls -1 *.param)
fi

