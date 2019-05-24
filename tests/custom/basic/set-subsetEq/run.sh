rm -rf conjure-output
conjure solve -ax --channelling=no --smart-filenames *.essence *.param
grep Node conjure-output/*info
rm -rf conjure-output
