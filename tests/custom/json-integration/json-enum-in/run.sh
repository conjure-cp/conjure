rm -rf conjure-output

conjure solve m.essence data.json --output-format=json --line-width=40 --copy-solutions=no --savilerow-options -O0 --number-of-solutions=all --output-format=json --solutions-in-one-file

cat conjure-output/*.solutions.json

rm -rf conjure-output
