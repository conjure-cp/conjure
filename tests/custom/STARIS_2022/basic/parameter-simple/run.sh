rm -rf conjure-output *.solution *.stats.json
conjure solve *.essence *.param 
cat *.solution
rm -rf conjure-output *.solution *.stats.json
