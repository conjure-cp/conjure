

rm -rf Occurrence Explicit


# Occurrence
conjure solve *.essence \
    -aai \
    --channelling=no \
    --copy-solutions=no \
    --number-of-solutions=all \
    --responses 1,1 \
    -o Occurrence

grep letting Occurrence/*.solution


# Explicit
conjure solve *.essence \
    -aai \
    --channelling=no \
    --copy-solutions=no \
    --number-of-solutions=all \
    --responses 2,2 \
    -o Explicit

grep letting Explicit/*.solution


rm -rf Occurrence Explicit

