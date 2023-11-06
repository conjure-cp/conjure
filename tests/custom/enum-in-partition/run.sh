rm -rf conjure-output *.solution *.solution.json
conjure solve eip.essence --output-format=json
cat *.solution *.solution.json
rm -rf conjure-output *.solution *.solution.json
