
rm -rf problem-*.* conjure-output

echo "$ conjure parameter-generator problem.essence"
conjure parameter-generator problem.essence
for file in problem-*.*; do
    echo "File: $file"
    cat $file
    echo ""
    echo "--------------------"
    echo ""
    echo ""
done

rm -rf problem-*.* conjure-output

echo "$ conjure parameter-generator problem.essence --MININT -10 --MAXINT 50"
conjure parameter-generator problem.essence --MININT -10 --MAXINT 50
for file in problem-*.*; do
    echo "File: $file"
    cat $file
    echo ""
    echo "--------------------"
    echo ""
    echo ""
done

rm -rf problem-*.* conjure-output
