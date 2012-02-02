
repeats=$1
timeout=$2

# echo $repeats
# echo $timeout

parallel -j3 runsavilerow                           \
    -in-eprime {2}                                  \
    -in-param {1}                                   \
    -out-minion {1.}-{2.}-{3}.minion                \
    -out-solution {1.}-{2.}-{3}.solution            \
    -minion-options \"-timelimit $timeout\"         \
    ::: *.param                                     \
    ::: *.eprime                                    \
    ::: `seq 1 $repeats`
