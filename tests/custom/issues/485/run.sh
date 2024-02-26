conjure parameter-generator model.essence --MAXINT=20
cat model-instanceGenerator.essence
cat model-instanceRepair.essence

conjure -ac model.essence
conjure -ac model-instanceGenerator.essence
conjure -ac model-instanceRepair.essence

rm -rf model-* conjure-output
