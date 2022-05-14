rm -rf conjure-output
conjure solve 511.essence --number-of-solutions=all
cat conjure-output/model000001.eprime | head -n8
cat *.solution
rm -rf conjure-output *.solution
