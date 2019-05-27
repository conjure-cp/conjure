rm -rf conjure-output

conjure solve -ax --channelling=no --smart-filenames *.essence *.param --savilerow-options -O0
grep Node conjure-output/*info
grep -i cse conjure-output/*eprime
rm conjure-output/*info

conjure solve -ax --channelling=no --smart-filenames *.essence *.param --savilerow-options -O1
grep Node conjure-output/*info
grep -i cse conjure-output/*eprime
rm conjure-output/*info

conjure solve -ax --channelling=no --smart-filenames *.essence *.param --savilerow-options -O2
grep Node conjure-output/*info
grep -i cse conjure-output/*eprime
rm conjure-output/*info

conjure solve -ax --channelling=no --smart-filenames *.essence *.param --savilerow-options -O3
grep Node conjure-output/*info
grep -i cse conjure-output/*eprime
rm conjure-output/*info

rm -rf conjure-output
