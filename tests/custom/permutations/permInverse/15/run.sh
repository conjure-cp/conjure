
rm -rf conjure-output *.solution *.stats.json
conjure solve --number-of-solutions=all --unnamed-symmetry-breaking=Complete-Consecutive-Altogether unnamed-set-of-set.essence
rm -rf conjure-output *.solution *.stats.json

