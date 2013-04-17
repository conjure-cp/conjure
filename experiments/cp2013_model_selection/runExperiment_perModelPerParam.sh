#/bin/bash

set -o nounset

ESSENCE="$1"
EPRIME="$2"
PARAM="$3"

DIR=$(dirname $ESSENCE)
EPRIMEBASE=${EPRIME%.eprime}
PARAMBASE=${PARAM%.param}

OUT_PARAM="${EPRIMEBASE}-${PARAMBASE}.eprime-param"
OUT_MINION="${EPRIMEBASE}-${PARAMBASE}.eprime-minion"
OUT_MINION_STATS="${EPRIMEBASE}-${PARAMBASE}.eprime-minion-stats"
OUT_SR_SOLUTION="${EPRIMEBASE}-${PARAMBASE}.eprime-solution"


MSG_TEMPLATE="$ESSENCE $EPRIME $PARAM"

FAIL_FILE="fails.txt"

rm -f "$FAIL_FILE"


RESULTOF_REFINEPARAM=0
MSG_REFINEPARAM="[refineParam] $MSG_TEMPLATE"
echo "$MSG_REFINEPARAM"
conjure                                                                 \
    --mode       refineParam                                            \
    --in-essence $ESSENCE                                               \
    --in-eprime  $EPRIME                                                \
    --in-essence-param $PARAM                                           \
    --out-eprime-param $OUT_PARAM
RESULTOF_REFINEPARAM=$?
if (( $RESULTOF_REFINEPARAM != 0 )) ; then
    echo "$MSG_REFINEPARAM" >> "$FAIL_FILE"
    exit 1
fi


RESULTOF_SAVILEROW=0
MSG_SAVILEROW="[savilerow] $MSG_TEMPLATE"
echo "$MSG_SAVILEROW"
savilerow                                                               \
    -in-eprime    $EPRIME                                               \
    -in-param     $OUT_PARAM                                            \
    -out-minion   $OUT_MINION                                           \
    -out-solution $OUT_SR_SOLUTION                                      \
    -boundvars                                                          \
    -minion-options "-timelimit 600"
RESULTOF_SAVILEROW=$?
if (( $RESULTOF_SAVILEROW != 0 )) ; then
    echo "$MSG_SAVILEROW" >> "$FAIL_FILE"
    exit 1
fi


RESULTOF_MINIONSTATS=0
MSG_MINIONSTATS="[minionStats] $MSG_TEMPLATE"
echo "$MSG_MINIONSTATS"
minion -instancestats $OUT_MINION > $OUT_MINION_STATS
RESULTOF_MINIONSTATS=$?
if (( $RESULTOF_MINIONSTATS != 0 )) ; then
    echo "$MSG_MINIONSTATS" >> "$FAIL_FILE"
    exit 1
fi


