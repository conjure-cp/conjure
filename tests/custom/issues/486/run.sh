rm -rf conjure-output *.solution *.stats.json
conjure solve model.essence --copy-solutions=no
cat conjure-output/*.solution
rm -rf conjure-output *.solution *.stats.json
