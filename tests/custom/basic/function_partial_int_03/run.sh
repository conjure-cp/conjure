rm -rf conjure-output *.solution *.stats.json
conjure solve -ax --channelling=no *.essence --number-of-solutions=all --copy-solutions=no
ls -1 conjure-output | LC_ALL=C sort
grep letting conjure-output/*.solution | LC_ALL=C sort
rm -rf conjure-output *.solution *.stats.json
