
rm -rf problem-*.* conjure-output

echo "$ conjure parameter-generator problem.essence"
conjure parameter-generator problem.essence --MININT -2 --MAXINT 10
conjure modelling -ac problem-instanceGenerator.essence
# write a test.param file and uncomment the following to test more of the functionality...
# conjure solve problem-instanceGenerator.essence test.param
for file in problem-*.*; do
    echo "File: $file"
    cat $file
    echo ""
    echo "--------------------"
    echo ""
    echo ""
done

rm -rf problem-*.* conjure-output
