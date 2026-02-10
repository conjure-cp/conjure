rm -rf conjure-output *.solution *.stats.json
conjure solve *.essence --number-of-solutions=all
cat *.solution
rm -rf conjure-output *.solution *.stats.json
