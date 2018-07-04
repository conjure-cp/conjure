
rm -rf conjure-output

conjure solve sbo.essence --number-of-solutions=all --copy-solutions=no --solver minion

grep 'letting a' conjure-output/*.solution | cut -d ':' -f 2 | LC_ALL=C sort

conjure solve sbo.essence --number-of-solutions=all --copy-solutions=no --solver lingeling

grep 'letting a' conjure-output/*.solution | cut -d ':' -f 2 | LC_ALL=C sort

rm -rf conjure-output
