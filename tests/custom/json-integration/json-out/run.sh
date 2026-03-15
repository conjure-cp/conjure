
rm -rf conjure-output *.solution *.stats.json

conjure solve test.essence --output-format=json --line-width=40 --copy-solutions=no

cat conjure-output/model000001-solution000001.solution.json

rm -rf conjure-output *.solution *.stats.json

