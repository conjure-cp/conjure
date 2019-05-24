rm -rf conjure-output
conjure solve -ax --channelling=no --smart-filenames *.essence *.param
grep Node conjure-output/*info
conjure solve -ax --channelling=no --smart-filenames *.essence *.param --savilerow-options -O0
grep Node conjure-output/*info
rm -rf conjure-output
