rm -rf conjure-output
conjure solve -ax 503.essence --copy-solutions=no --line-width=100000
grep '^find' conjure-output/*.eprime
grep '^letting' conjure-output/*.solution
rm -rf conjure-output
