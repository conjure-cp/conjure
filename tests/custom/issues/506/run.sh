rm -rf conjure-output
conjure solve model.essence test.param
cat *.solution
rm -rf conjure-output *.solution
