
rm -rf conjure-output *.solution *.stats.json

conjure solve test.essence param.json --output-format=json --line-width=40 --copy-solutions=no --savilerow-options -O0

cat conjure-output/model000001-param-solution000001.solution.json

rm -rf conjure-output *.solution *.stats.json
