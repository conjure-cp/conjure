
repeats=$1
timeout=$2

# echo $repeats
# echo $timeout

if (ls *.param > /dev/null 2> /dev/null ) then
    parallel -j3 runsavilerow                           \
        -in-eprime {2}                                  \
        -in-param {1}                                   \
        -out-minion {1.}-{2.}-run{3}.minion             \
        -out-solution {1.}-{2.}-run{3}.solution         \
        -minion-options \"-timelimit $timeout\"         \
        ::: *.param                                     \
        ::: *.eprime                                    \
        ::: `seq 1 $repeats`
else
    parallel -j3 runsavilerow                           \
        -in-eprime {1}                                  \
        -out-minion {1.}-run{2.}.minion                 \
        -out-solution {1.}-run{2.}.solution             \
        -minion-options \"-timelimit $timeout\"         \
        ::: *.eprime                                    \
        ::: `seq 1 $repeats`    
fi

