
rm -rf conjure-output

conjure solve test.essence param.json --output-format=json --line-width=40 --copy-solutions=no

cat conjure-output/model000001-param-solution000001.solution.json

rm -rf conjure-output
