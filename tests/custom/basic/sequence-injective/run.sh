rm -rf conjure-output
conjure solve *.essence --copy-solutions=no --number-of-solutions=all
head -n12 conjure-output/*.eprime
grep letting conjure-output/*.solution
rm -rf conjure-output
