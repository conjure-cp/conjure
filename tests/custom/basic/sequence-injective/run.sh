rm -rf conjure-output *.solution *.stats.json
conjure solve *.essence --copy-solutions=no --number-of-solutions=all
head -n12 conjure-output/*.eprime
grep letting conjure-output/*.solution | LC_ALL=C sort
rm -rf conjure-output *.solution *.stats.json
