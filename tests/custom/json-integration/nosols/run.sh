rm -rf conjure-output
conjure solve nosols.essence --number-of-solutions=all --solutions-in-one-file --output-format=json
cat nosols.solutions.json
rm -rf conjure-output nosols.solutions.json
