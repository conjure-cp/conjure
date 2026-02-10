rm -rf conjure-output *.solution *.stats.json *.solution.json
conjure solve --savilerow-options -O0 *.essence --number-of-solutions=all
cat *.solution
rm -rf conjure-output *.solution *.stats.json *.solution.json
