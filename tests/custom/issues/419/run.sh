conjure modelling -ac *.essence
cat conjure-output/model000001.eprime | grep -v "^[$]"
rm -rf conjure-output
