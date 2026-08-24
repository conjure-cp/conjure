rm -rf conjure-output *.solution *.stats.json
conjure solve mset-union-equality.essence
conjure solve mset-union-cardinality.essence
cat *.solution
rm -rf conjure-output *.solution *.stats.json
