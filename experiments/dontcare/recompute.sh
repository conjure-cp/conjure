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
    parallel {1} {2//} ::: conjureInDir_usesDontCare conjureInDir_noDontCare ::: $(find . -name "*.essence")
}
export -f conjureInAllDirs


function srOne() {
    BASE="$1"
    DIR="$( cd "$( dirname "${BASE}.eprime" )" && pwd )"
    echo "Running Savile Row on ${BASE}"
    savilerow                                                           \
        -minion-options "-timelimit 600"                                \
        -boundvars                                                      \
        -deletevars                                                     \
        -run-minion     minion                                          \
        -in-eprime      ${BASE}.eprime                                  \
        -out-minion     ${BASE}.minion                                  \
        -out-info       ${BASE}.info                                    \
        -out-solution   ${BASE}.eprime-solution 2> ${BASE}.stderr | tee ${BASE}.stdout
    rm -f "${BASE}.minion.aux" "${BASE}.infor"

    # echo "Running Conjure on the solutions"
    # conjure                                                             \
    #     --mode translateSolution                                        \
    #     --in-essence            ${DIR}/../*.essence                     \
    #     --in-eprime             ${BASE}.eprime                          \
    #     --in-eprime-solution    ${BASE}.eprime-solution                 \
    #     --out-essence-solution  ${BASE}.solution
}
export -f srOne


function srAll() {
    parallel srOne {.} ::: $(find . -name "*.eprime")
}
export -f srAll

function report_unsat() {
    grep MinionSolutionsFound:0 $(find . -name "*.info") | cut -d ':' -f 1 | cut -d '.' -f 1
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
    parallel -j1 "echo removing {} ; rm {}" ::: $(find . -size 0 )
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

