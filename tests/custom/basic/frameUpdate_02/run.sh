
rm -rf output-*

for i in {1..4}; do

    conjure solve frameUpdate_02.essence \
        -aai \
        --channelling=no \
        --copy-solutions=no \
        --number-of-solutions=all \
        --responses $i,$i \
        -o output-$i
    grep letting output-$i/*.solution
    echo ""
    echo ""
    echo ""

done;

rm -rf output-*

