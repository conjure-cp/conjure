rm -rf conjure-output
conjure solve -ac --smart-filenames --channelling=no *.essence --validate-solutions
for file in conjure-output/*.eprime conjure-output/*.solution; do
    echo "File: $file"
    cat $file | grep -v '\$'
    echo "--------------------"
    echo ""
done
rm -rf conjure-output *.solution
