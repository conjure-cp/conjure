

rm -rf Occurrence Explicit


# Occurrence
conjure solve frameUpdate_01.essence \
    -aai \
    --channelling=no \
    --copy-solutions=no \
    --number-of-solutions=all \
    --responses 1,1 \
    -o Occurrence

cat Occurrence/*.solution


# Explicit
conjure solve frameUpdate_01.essence \
    -aai \
    --channelling=no \
    --copy-solutions=no \
    --number-of-solutions=all \
    --responses 1,1 \
    -o Explicit

cat Explicit/*.solution


rm -rf Occurrence Explicit

