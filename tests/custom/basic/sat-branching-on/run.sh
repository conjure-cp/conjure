
rm -rf conjure-output *.solution *.stats.json

conjure solve sbo.essence --number-of-solutions=all --copy-solutions=no --solver minion

grep 'letting a' conjure-output/*.solution | cut -d ':' -f 2 | LC_ALL=C sort
rm -rf conjure-output *.solution *.stats.json/*.solution

conjure solve sbo.essence --number-of-solutions=all --copy-solutions=no --solver kissat

grep 'letting a' conjure-output/*.solution | cut -d ':' -f 2 | LC_ALL=C sort

rm -rf conjure-output *.solution *.stats.json
