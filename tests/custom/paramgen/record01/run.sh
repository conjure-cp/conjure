
rm -rf problem-gen.* problem-gen-test.solution conjure-output

echo "$ conjure parameter-generator problem.essence --essence-out problem-gen.essence"
conjure parameter-generator problem.essence --essence-out problem-gen.essence
conjure modelling -ac problem-gen.essence
# write a test.param file and uncomment the following to test more of the functionality...
# conjure solve problem-gen.essence test.param
# for file in problem-gen.* problem-gen-test.solution; do
for file in problem-gen.* ; do
    echo "File: $file"
    cat $file
    echo ""
    echo "--------------------"
    echo ""
    echo ""
done

rm -rf problem-gen.* problem-gen-test.solution conjure-output

echo "$ conjure parameter-generator problem.essence --essence-out problem-gen.essence --MININT -10 --MAXINT 50"
conjure parameter-generator problem.essence --essence-out problem-gen.essence --MININT -10 --MAXINT 50
conjure modelling -ac problem-gen.essence
# write a test.param file and uncomment the following to test more of the functionality...
# conjure solve problem-gen.essence test.param
# for file in problem-gen.* problem-gen-test.solution; do
for file in problem-gen.* ; do
    echo "File: $file"
    cat $file
    echo ""
    echo "--------------------"
    echo ""
    echo ""
done

rm -rf problem-gen.* problem-gen-test.solution conjure-output
