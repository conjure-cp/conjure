rm -rf conjure-output *.solutions*
conjure solve *.essence --number-of-solutions=all --output-format=json --solutions-in-one-file
cat *.solutions.json
rm -rf conjure-output *.solutions*
