rm -rf conjure-output
conjure solve -ax --channelling=no *.essence --number-of-solutions=all
ls -1 conjure-output | LC_ALL=C sort
grep letting conjure-output/*.solution | LC_ALL=C
rm -rf conjure-output
