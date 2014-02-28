#!/bin/bash

function relpath {
    python -c "import os.path; print os.path.relpath('$2', '$1')"
}
export -f relpath

function conjureInDir_noDontCare() {
    OLDWD=$(pwd)
    pushd "$1" > /dev/null
    echo "conjureInDir_usesDontCare working in directory: $(relpath $OLDWD $1)"
    conjure --mode df-no-channelling --in *.essence --no-dontCare --out noDontCare   +RTS -s 2> "noDontCare.conjure-stderr"   | tee "noDontCare.conjure-stdout"
    popd > /dev/null
}
export -f conjureInDir_noDontCare

function conjureInDir_usesDontCare() {
    OLDWD=$(pwd)
    pushd "$1" > /dev/null
    echo "conjureInDir_usesDontCare working in directory: $(relpath $OLDWD $1)"
    conjure --mode df-no-channelling --in *.essence               --out usesDontCare +RTS -s 2> "usesDontCare.conjure-stderr" | tee "usesDontCare.conjure-stdout"
    popd > /dev/null
}
export -f conjureInDir_usesDontCare


function conjureInAllDirs() {
    parallel --no-notice {1} {2//} ::: conjureInDir_usesDontCare conjureInDir_noDontCare ::: $(find . -name "*.essence" | grep 'Nested-Types' -v)
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
            -out-solution   ${OUTPUT}.eprime-solution 2> ${OUTPUT}.savilerow-stderr | tee ${OUTPUT}.savilerow-stdout
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
            -out-solution   ${OUTPUT}.eprime-solution 2> ${OUTPUT}.savilerow-stderr | tee ${OUTPUT}.savilerow-stdout
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
    parallel --no-notice -j1 echo {1.} "none" "none" ::: $(find all-combinations -name "*.eprime")                     >> argslist.txt
    parallel --no-notice -j1 echo {1.} {2/.}  {2}    ::: dominating-queens/*/*.eprime  ::: dominating-queens/*.param   >> argslist.txt
    # parallel --no-notice -j1 echo {1.} "none" "none" ::: Nested-Types/*/*/*.eprime                                     >> argslist.txt
    parallel --no-notice --colsep ' ' srOne {1} {2} {3} :::: argslist.txt
}
export -f srAll

function report_timeout() {
    grep "MinionTimeOut:1" $(find . -name "*.info") | cut -d ':' -f 1
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


# function recompute() {
#     echo "recomputing..."
#     pushd all-combinations ; runhaskell create_essences.hs ; popd
#     conjureInAllDirs
#     srAll
#     parallel --no-notice -j1 "echo removing {} ; rm {}" ::: $(find . -size 0 )
#     echo "recomputed, happy?"
# }
# export -f recompute


function clean() {
    rm -rf   */*DontCare   */*.stdout   */*.stderr
    rm -rf */*/*DontCare */*/*.stdout */*/*.stderr
}
export -f



function conjure_compact() {
    echo "conjure_compact working on: $1 $2"
    mkdir -p $2/compact

    FLAG=""
    if [ $1 = "noDontCare" ]; then
        FLAG="--no-dontCare"
    fi

    conjure $FLAG                       \
        --mode compact                  \
        --in  $2/$2.essence             \
        --out $2/compact/$1.eprime      \
        +RTS -s 2> $2/compact/$1.conjure-stderr | tee $2/compact/$1conjure-stdout
}
export -f conjure_compact


function conjure_compact_all_solutions {
    ESSENCE=$1/$1
    MODEL=$1/compact/$1

    savilerow                                                               \
        -in-eprime      $MODEL.eprime                                       \
        -out-minion     $MODEL.minion                                       \
        -out-solution   $MODEL.eprime-solution                              \
        -timelimit      3600000                                             \
        -minion-options "-cpulimit 3600"                                    \
        -run-minion minion                                                  \
        -all-solutions 2> $MODEL.savilerow-stderr | tee $MODEL.savilerow-stdout

    cmd="conjure
            --mode translateSolution
            --in-essence            $ESSENCE.essence
            --in-eprime             $MODEL.eprime
            --in-eprime-solution    {}
            --out-essence-solution  {}.solution"
    parallel --no-notice $cmd ::: $MODEL.eprime-solution.*
}
export -f conjure_compact_all_solutions


function conjure_compact_all_solutions_count {
    ESSENCE=$2/$2
    MODEL=$2/compact/$1

    savilerow                                                               \
        -in-eprime      $MODEL.eprime                                       \
        -out-minion     $MODEL.minion                                       \
        -timelimit      3600000 2> $MODEL.savilerow-stderr | tee $MODEL.savilerow-stdout

    minion                                                                  \
        -cpulimit 3600                                                      \
        $MODEL.minion                                                       \
        -findallsols                                                        \
        -noprintsols 2> $MODEL.minion-stderr | tee $MODEL.minion-stdout
}
export -f conjure_compact_all_solutions_count








# experiment plan
# - create essence files for "all combinations"
# - conjure_compact on each (with and without dontCare)
# - conjure_compact_all_solutions_count on each
# - conjure_all on each (with and without dontCare)
# - conjure_all_solve on each


# directory structure

# - all-combinations

# - all-combinations/<name>/<name>.essence

# - all-combinations/<name>/compact.conjure-stderr
# - all-combinations/<name>/compact.conjure-stdout
# - all-combinations/<name>/compact/noDontCare.eprime
# - all-combinations/<name>/compact/noDontCare.minion
# - all-combinations/<name>/compact/noDontCare.savilerow-stdout
# - all-combinations/<name>/compact/noDontCare.savilerow-stderr
# - all-combinations/<name>/compact/noDontCare.minion-stdout
# - all-combinations/<name>/compact/noDontCare.minion-stderr

# - all-combinations/<name>/noDontCare.conjure-stderr
# - all-combinations/<name>/noDontCare.conjure-stdout
# - all-combinations/<name>/noDontCare/<number>.eprime
# - all-combinations/<name>/noDontCare/<number>.minion
# - all-combinations/<name>/noDontCare/<number>.eprime.solution

# - all-combinations/<name>/usesDontCare.conjure-stderr
# - all-combinations/<name>/usesDontCare.conjure-stdout
# - all-combinations/<name>/usesDontCare/<number>.eprime
# - all-combinations/<name>/usesDontCare/<number>.minion
# - all-combinations/<name>/usesDontCare/<number>.eprime.solution





echo "recomputing..."

pushd all-combinations

runhaskell create_essences.hs

# conjure_compact
parallel --no-notice conjure_compact {1} {2//} ::: noDontCare usesDontCare ::: */*.essence

# conjure_compact_all_solutions_count
parallel --no-notice conjure_compact_all_solutions_count {1} {2//} ::: noDontCare usesDontCare ::: */*.essence

# conjure_all
parallel --no-notice {1} {2//} ::: conjureInDir_noDontCare conjureInDir_usesDontCare ::: */*.essence

# conjure_all_solve
parallel --no-notice srOne {} "none" "none" ::: */*DontCare/*.eprime

if [ $(find . -size 0 | wc -l) -gt 0 ] ; then
    parallel --no-notice -j1 "echo removing {} ; rm {}" ::: $(find . -size 0)
fi
parallel --no-notice -j1 "echo removing {} ; rm {}" ::: $(find . -name "*.minion.aux")

popd


echo "recomputed, happy?"




