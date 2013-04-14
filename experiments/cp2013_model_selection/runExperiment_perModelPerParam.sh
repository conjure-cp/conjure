#/bin/bash

set -o nounset

ESSENCE="$1"
EPRIME="$2"
PARAM="$3"

DIR=$(dirname $ESSENCE)

OUT_PARAM="blah"
OUT_MINION="blah"
OUT_MINION_STATS="blah"
OUT_SR_SOLUTION="blah"
OUT_SOLUTION="blah"


MSG_TEMPLATE="$ESSENCE $EPRIME $PARAM"

FAIL_FILE="fail"

rm -f "$FAIL_FILE"
touch "$FAIL_FILE"


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
    -out-solution $OUT_SOLUTION                                         \
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


