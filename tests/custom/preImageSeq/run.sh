rm -rf conjure-output *.solution *.stats.json
conjure modelling *.essence
head -n20 conjure-output/model000001.eprime
rm -rf conjure-output *.solution *.stats.json
