rm -rf conjure-output *.solution *.stats.json
conjure solve *.essence *.json
cat *.solution
rm -rf conjure-output *.solution *.stats.json
