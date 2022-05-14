rm -rf conjure-output
conjure solve 511.essence --number-of-solutions=all
cat *.solution
rm -rf conjure-output *.solution
