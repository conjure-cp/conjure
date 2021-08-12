rm -rf conjure-output *.solution
conjure solve json-solution.essence n4.param --output-format=json
cat conjure-output/model000001-n4-solution000001.solution.json
rm -rf conjure-output *.solution
