#!/bin/bash


function conjureInDir() {
    pushd "$1"
    echo "conjureInDir working in directory: $(pwd)"
    conjure --mode df --in *.essence --no-dontCare --out noDontCare   +RTS -s 2> "noDontCare.stderr"   | tee "noDontCare.stdout"
    conjure --mode df --in *.essence               --out usesDontCare +RTS -s 2> "usesDontCare.stderr" | tee "usesDontCare.stdout"
    popd
}
export -f conjureInDir


function conjureInAllDirs() {
    parallel conjureInDir ::: */*
}
export -f conjureInAllDirs


function srOne() {
    BASE="$1"
    echo "Running Savile Row on ${BASE}"
    savilerow                                                           \
        -boundvars                                                      \
        -deletevars                                                     \
        -run-minion     minion                                          \
        -in-eprime      "${BASE}.eprime"                                \
        -out-minion     "${BASE}.minion"                                \
        -out-info       "${BASE}.info"                                  \
        -out-solution   "${BASE}.eprime-solution" 2> "${BASE}.stderr" | tee "${BASE}.stdout"
    rm -f "${BASE}.minion.aux" "${BASE}.infor"
}
export -f srOne


function srAll() {
    parallel srOne {.} ::: $(find . -name "*.eprime")
}
export -f srAll

function report_unsat() {
    grep MinionSolutionsFound:0 */*/*/*.info | cut -d ':' -f 1 | cut -d '.' -f 1
}
export -f report_unsat


function report_nodes() {
    grep MinionNodes */*/*/*.info
}
export -f report_nodes


function report_minionTimes() {
    grep MinionSolveTime */*/*/*.info
}
export -f report_minionTimes


function recompute() {
    echo "recomputing..."
    conjureInAllDirs
    srAll
    echo "recomputed, happy?"
}
export -f recompute


function clean() {
    rm -rf */*/*DontCare    \
           */*/*.stdout     \
           */*/*.stderr
}
export -f

