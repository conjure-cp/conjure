
echo "$ conjure parameter-generator function.essence --essence-out function-gen.essence"
conjure parameter-generator function.essence --essence-out function-gen.essence
for file in function-gen.*; do
    echo "File: $file"
    cat $file
    echo ""
    echo "--------------------"
    echo ""
    echo ""
done

echo "$ conjure parameter-generator function.essence --essence-out function-gen.essence --MININT -10 --MAXINT 50"
conjure parameter-generator function.essence --essence-out function-gen.essence --MININT -10 --MAXINT 50
for file in function-gen.*; do
    echo "File: $file"
    cat $file
    echo ""
    echo "--------------------"
    echo ""
    echo ""
done

rm -f function-gen.*
