rm -rf conjure-output
conjure modelling -ac dominance.essence
cat conjure-output/model000001.eprime | grep -v '\$'
rm -rf conjure-output

