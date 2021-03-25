rm -rf conjure-output
conjure solve allDiffDecomp.essence --copy-solutions=no --number-of-solutions 50
grep letting conjure-output/*.solution
rm -rf conjure-output
