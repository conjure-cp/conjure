rm -rf conjure-output
conjure solve jsons.essence --copy-solutions=no --number-of-solutions=all --solutions-in-one-file --output-format=json
cat conjure-output/model000001.solutions.json
rm -rf conjure-output
