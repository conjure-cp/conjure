rm -rf conjure-output *.solution *.stats.json
conjure solve 1.essence --number-of-solutions=all
cat *.solution
rm -rf conjure-output *.solution *.stats.json
conjure solve 2.essence --number-of-solutions=all
cat *.solution
rm -rf conjure-output *.solution *.stats.json

