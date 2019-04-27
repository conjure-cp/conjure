
echo "$ conjure parameter-generator problem.essence --essence-out problem-gen.essence"
conjure parameter-generator problem.essence --essence-out problem-gen.essence
for file in problem-gen.*; do
    echo "File: $file"
    cat $file
    echo ""
    echo "--------------------"
    echo ""
    echo ""
done

echo "$ conjure parameter-generator problem.essence --essence-out problem-gen.essence --MININT -10 --MAXINT 50"
conjure parameter-generator problem.essence --essence-out problem-gen.essence --MININT -10 --MAXINT 50
for file in problem-gen.*; do
    echo "File: $file"
    cat $file
    echo ""
    echo "--------------------"
    echo ""
    echo ""
done

rm -f problem-gen.*
