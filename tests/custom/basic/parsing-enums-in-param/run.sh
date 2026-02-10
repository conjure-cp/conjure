rm -rf conjure-output *.solution *.stats.json
conjure solve *.essence *.param --line-width 80 --number-of-solutions=all
for file in conjure-output/*.eprime conjure-output/*.solution; do
    echo "File: $file"
    cat $file | grep -v "^[$]"
    echo "--------------------"
    echo ""
done
rm -rf conjure-output *.solution *.stats.json
