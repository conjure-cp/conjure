rm -rf conjure-output
conjure solve *.essence --copy-solutions=no
head -n12 conjure-output/*.eprime
rm -rf conjure-output
