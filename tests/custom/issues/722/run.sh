rm -rf conjure-output *.solution *.stats.json
conjure solve 1.essence
conjure solve 2.essence --number-of-solutions=10
cat *.solution
rm -rf conjure-output *.solution *.stats.json

