rm -rf conjure-output
conjure solve model.essence --copy-solutions=no
cat conjure-output/*.solution
rm -rf conjure-output
