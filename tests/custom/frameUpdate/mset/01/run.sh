

rm -rf WithFlags WithRepetition


# WithFlags
conjure solve *.essence \
    --frameUpdate=decomposition \
    -aai \
    --channelling=no \
    --copy-solutions=no \
    --number-of-solutions=all \
    --responses 1,1 \
    -o WithFlags

grep letting WithFlags/*.solution


# WithRepetition
conjure solve *.essence \
    --frameUpdate=decomposition \
    -aai \
    --channelling=no \
    --copy-solutions=no \
    --number-of-solutions=all \
    --responses 2,2 \
    -o WithRepetition

grep letting WithRepetition/*.solution


rm -rf WithFlags WithRepetition

