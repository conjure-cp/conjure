rm -rf conjure-output *.solution *.stats.json
conjure solve allDiffDecomp.essence --copy-solutions=no --number-of-solutions 50
grep letting conjure-output/*.solution | LC_ALL=C sort
rm -rf conjure-output *.solution *.stats.json
