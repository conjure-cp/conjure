rm -rf conjure-output *.solution *.stats.json
conjure solve model.essence test.param
cat *.solution
rm -rf conjure-output *.solution *.stats.json
