rm -rf conjure-output *.solution *.stats.json large.param large.eprime-param
conjure modelling -ac knapsack.essence
cp large.param.gz copy-large.param.gz
gunzip large.param.gz
conjure translate-param --eprime conjure-output/model000001.eprime --essence-param large.param
wc -l knapsack.essence conjure-output/model000001.eprime large.param large.eprime-param | sed 's/^ *//'
cat conjure-output/*.eprime | grep -v "^[$]"
mv copy-large.param.gz large.param.gz
rm -rf conjure-output *.solution *.stats.json large.param large.eprime-param
