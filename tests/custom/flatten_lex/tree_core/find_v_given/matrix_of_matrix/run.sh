conjure modelling *.essence
cat conjure-output/*.eprime | grep -v "^[$]"
rm -rf conjure-output *.solution *.stats.json
