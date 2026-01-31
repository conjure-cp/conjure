rm -rf conjure-output
conjure solve *.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no -ax --channelling=no
ls -1 conjure-output/*.solutions.json
cat conjure-output/model000001.solutions.json
rm -rf conjure-output
