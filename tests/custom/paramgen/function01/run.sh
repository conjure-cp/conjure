
rm -rf function-gen.* function-gen-test.solution conjure-output

echo "$ conjure parameter-generator function.essence --essence-out function-gen.essence"
conjure parameter-generator function.essence --essence-out function-gen.essence
conjure solve function-gen.essence test.param
for file in function-gen.* function-gen-test.solution; do
    echo "File: $file"
    cat $file
    echo ""
    echo "--------------------"
    echo ""
    echo ""
done

rm -rf function-gen.* function-gen-test.solution conjure-output

echo "$ conjure parameter-generator function.essence --essence-out function-gen.essence --MININT -10 --MAXINT 50"
conjure parameter-generator function.essence --essence-out function-gen.essence --MININT -10 --MAXINT 50
conjure solve function-gen.essence test.param
for file in function-gen.* function-gen-test.solution; do
    echo "File: $file"
    cat $file
    echo ""
    echo "--------------------"
    echo ""
    echo ""
done

rm -rf function-gen.* function-gen-test.solution conjure-output
