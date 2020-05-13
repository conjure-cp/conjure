rm -rf conjure-output
conjure *.essence
cat conjure-output/model000001.eprime | grep "^[^$]"
rm -rf conjure-output
