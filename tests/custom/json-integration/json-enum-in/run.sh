
rm -rf conjure-output

conjure solve m.essence data.json --output-format=json --line-width=40 --copy-solutions=no --savilerow-options -O0 --number-of-solutions=all --output-format=json

ls -ls conjure-output

rm -rf conjure-output
