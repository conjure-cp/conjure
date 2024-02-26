conjure solve works.essence test.param
cat works*.solution

echo ""
echo ""

conjure solve regression.essence test.param
cat regression*.solution

rm -rf conjure-output *.solution
