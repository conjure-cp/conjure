rm -rf conjure-output *.solution *.stats.json
# conjure solve *.essence --validate-solutions --line-width 80
conjure solve *.essence --line-width 80
for file in conjure-output/*.eprime conjure-output/*.solution; do
    echo "File: $file"
    cat $file | grep -v "^[$]"
    echo "--------------------"
    echo ""
done
rm -rf conjure-output *.solution *.stats.json
