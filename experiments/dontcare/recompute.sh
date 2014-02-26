#!/bin/bash


function conjureInDir_noDontCare() {
    pushd "$1" > /dev/null
    echo "conjureInDir_noDontCare working in directory: $(pwd)"
    conjure --mode df-no-channelling --in *.essence --no-dontCare --out noDontCare   +RTS -s 2> "noDontCare.stderr"   | tee "noDontCare.stdout"
    popd > /dev/null
}
export -f conjureInDir_noDontCare

function conjureInDir_usesDontCare() {
    pushd "$1" > /dev/null
    echo "conjureInDir_usesDontCare working in directory: $(pwd)"
    conjure --mode df-no-channelling --in *.essence               --out usesDontCare +RTS -s 2> "usesDontCare.stderr" | tee "usesDontCare.stdout"
    popd > /dev/null
}
export -f conjureInDir_usesDontCare


function conjureInAllDirs() {
    parallel --no-notice {1} {2//} ::: conjureInDir_usesDontCare conjureInDir_noDontCare ::: $(find . -name "*.essence")
}
export -f conjureInAllDirs


function srOne() {
    EPRIME="$1"
    PARAM="$2"
    PARAM_FULL="$3"
    if [ ${PARAM} = "none" ] ; then
        OUTPUT="$EPRIME"
        echo "Running Savile Row: ${OUTPUT}"
        savilerow                                                           \
            -timelimit      3600000                                         \
            -minion-options "-cpulimit 3600"                                \
            -boundvars                                                      \
            -deletevars                                                     \
            -preprocess     None                                            \
            -run-minion     minion                                          \
            -in-eprime      ${EPRIME}.eprime                                \
            -out-minion     ${OUTPUT}.minion                                \
            -out-info       ${OUTPUT}.info                                  \
            -out-solution   ${OUTPUT}.eprime-solution 2> ${OUTPUT}.stderr | tee ${OUTPUT}.stdout
        rm -f "${OUTPUT}.minion.aux" "${OUTPUT}.infor"
    else
        OUTPUT="$EPRIME-$PARAM"
        echo "Running Savile Row: ${OUTPUT} $PARAM_FULL"
        savilerow                                                           \
            -timelimit      3600000                                         \
            -minion-options "-cpulimit 3600"                                \
            -boundvars                                                      \
            -deletevars                                                     \
            -preprocess     None                                            \
            -run-minion     minion                                          \
            -in-eprime      ${EPRIME}.eprime                                \
            -in-param       ${PARAM_FULL}                                   \
            -out-minion     ${OUTPUT}.minion                                \
            -out-info       ${OUTPUT}.info                                  \
            -out-solution   ${OUTPUT}.eprime-solution 2> ${OUTPUT}.stderr | tee ${OUTPUT}.stdout
        rm -f "${OUTPUT}.minion.aux" "${OUTPUT}.infor"
    fi
}
export -f srOne


function srAll() {
    rm -f argslist.txt
    parallel --no-notice -j1 echo {1.} "none" "none" ::: Set-VarSize/*/*/*.eprime                                      >> argslist.txt
    parallel --no-notice -j1 echo {1.} "none" "none" ::: MSet-VarSize/*/*/*.eprime                                     >> argslist.txt
    parallel --no-notice -j1 echo {1.} "none" "none" ::: Relation-VarSize/*/*/*.eprime                                 >> argslist.txt
    parallel --no-notice -j1 echo {1.} "none" "none" ::: Function-Partial/*/*/*.eprime                                 >> argslist.txt
    parallel --no-notice -j1 echo {1.} "none" "none" ::: Partition-VarSize/*/*/*.eprime                                >> argslist.txt
    parallel --no-notice -j1 echo {1.} {2/.}  {2}    ::: dominating-queens/*/*.eprime  ::: dominating-queens/*.param   >> argslist.txt
    parallel --no-notice --colsep ' ' srOne {1} {2} {3} :::: argslist.txt
}
export -f srAll

function report_unsat() {
    grep MinionSolutionsFound:0 $(find . -name "*.info") | cut -d ':' -f 1
}
export -f report_unsat


function report_nodes() {
    grep MinionNodes $(find . -name "*.info")
}
export -f report_nodes


function report_minionTimes() {
    grep MinionSolveTime $(find . -name "*.info")
}
export -f report_minionTimes


function recompute() {
    echo "recomputing..."
    conjureInAllDirs
    srAll
    parallel --no-notice -j1 "echo removing {} ; rm {}" ::: $(find . -size 0 )
    echo "recomputed, happy?"
}
export -f recompute


function clean() {
    rm -rf */*DontCare       \
           */*.stdout        \
           */*.stderr
    rm -rf */*/*DontCare    \
           */*/*.stdout     \
           */*/*.stderr
}
export -f

