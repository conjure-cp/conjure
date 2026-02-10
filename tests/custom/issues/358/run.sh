rm -rf conjure-output *.solution *.stats.json
conjure solve *.essence
cat conjure-output/model000001.eprime | grep -v "^[$]"
cat *.solution
rm -rf conjure-output *.solution *.stats.json
