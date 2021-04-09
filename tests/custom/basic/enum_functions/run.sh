rm -rf conjure-output
conjure solve -ax --channelling=no *.essence *.param --number-of-solutions=all
ls -1 conjure-output
grep letting conjure-output/*.solution
rm -rf conjure-output
