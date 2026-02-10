rm -rf conjure-output *.solution *.stats.json
conjure solve nosols.essence --number-of-solutions=all --solutions-in-one-file --output-format=json
cat nosols.solutions.json
rm -rf conjure-output *.solution *.stats.json nosols.solutions.json nosols.stats.json
