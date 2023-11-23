rm -rf conjure-output *.eprime-param
conjure modelling -ac 553.essence
conjure translate-parameter --eprime conjure-output/model000001.eprime --essence-param p1.param
conjure translate-parameter --eprime conjure-output/model000001.eprime --essence-param p2.json
cat p1.eprime-param p2.eprime-param
rm -rf conjure-output *.eprime-param
