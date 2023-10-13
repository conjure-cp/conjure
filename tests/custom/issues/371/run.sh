rm -rf conjure-output

conjure modelling -ac 371-1.essence
cat conjure-output/model000001.eprime | grep -v "^[$]"
rm -rf conjure-output

conjure modelling -ac 371-1.essence
cat conjure-output/model000001.eprime | grep -v "^[$]"
rm -rf conjure-output
