rm -rf conjure-output *.solution *.stats.json *.solution.json
conjure solve --savilerow-options -O0 --solver chuffed *.essence
cat *.solution
rm -rf conjure-output *.solution *.stats.json *.solution.json
