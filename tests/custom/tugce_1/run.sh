rm -rf conjure-output *.solution *.solution.json
conjure solve --savilerow-options -O0 --solver chuffed *.essence
cat *.solution
rm -rf conjure-output *.solution *.solution.json
