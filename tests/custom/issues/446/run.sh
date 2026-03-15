rm -rf conjure-output *.solution *.stats.json
conjure solve model.essence inst.param 2>&1
rm -rf conjure-output *.solution *.stats.json
conjure solve model.essence inst2.param 2>&1
rm -rf conjure-output *.solution *.stats.json
