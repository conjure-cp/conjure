rm -rf conjure-output
conjure solve *.essence --number-of-solutions=all --solutions-in-one-file
cat conjure-output/*.eprime-solutions
cat conjure-output/*.solutions
rm -rf conjure-output test.solutions
