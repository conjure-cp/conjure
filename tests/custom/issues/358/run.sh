rm -rf conjure-output *.solution
conjure solve *.essence
head -n12 conjure-output/model000001.eprime
cat *.solution
rm -rf conjure-output *.solution
