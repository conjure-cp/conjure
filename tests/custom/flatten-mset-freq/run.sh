rm -rf conjure-output *.solution *.solution.json
conjure solve --savilerow-options -O0 *.essence --number-of-solutions=all
cat *.solution
rm -rf conjure-output *.solution *.solution.json
