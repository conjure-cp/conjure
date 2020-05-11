conjure parameter-generator ex.essence --MAXINT=20
cat ex-instanceGenerator.essence
cat ex-instanceRepair.essence

conjure -ac ex.essence
conjure -ac ex-instanceGenerator.essence
conjure -ac ex-instanceRepair.essence

rm -rf ex-* conjure-output
