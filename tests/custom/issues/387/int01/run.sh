rm -rf conjure-output *.solution *.stats.json
conjure solve -ax --smart-filenames --channelling=no *.essence --validate-solutions
cat conjure-output/*.solution
rm -rf conjure-output *.solution *.stats.json
