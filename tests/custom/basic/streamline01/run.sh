rm -rf conjure-output
conjure solve -ax --channelling=no *.essence --number-of-solutions=all
ls -1 conjure-output
grep letting conjure-output/*.solution
rm -rf conjure-output
