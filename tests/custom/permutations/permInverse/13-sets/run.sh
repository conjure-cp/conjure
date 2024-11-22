
rm -rf conjure-output

conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --channelling=no --responses=1,1 

cat conjure-output/model000001.solutions.json
rm -rf conjure-output
