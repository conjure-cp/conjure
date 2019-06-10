rm -rf conjure-output
conjure modelling -ac knapsack.essence
gunzip large.param.gz
conjure translate-param --eprime conjure-output/model000001.eprime --essence-param large.param
wc -l knapsack.essence conjure-output/model000001.eprime large.param large.eprime-param | sed 's/^ *//'
gzip large.param
rm -rf conjure-output large.eprime-param
