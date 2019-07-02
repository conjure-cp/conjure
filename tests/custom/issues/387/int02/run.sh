rm -rf conjure-output
conjure solve -ax --smart-filenames --channelling=no *.essence --validate-solutions
rm -rf conjure-output *.solution
