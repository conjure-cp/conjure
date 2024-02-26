rm -rf conjure-output
conjure solve -ax --channelling=no *.essence *.param --number-of-solutions=all --copy-solutions=no
ls -1 conjure-output | LC_ALL=C sort
grep letting conjure-output/*.solution | LC_ALL=C sort
rm -rf conjure-output
