rm -rf conjure-output *.solution generator.*
conjure parameter-generator model.essence --essence-out generator.essence
conjure solve generator.essence p.param
cat *.solution
rm -rf conjure-output *.solution generator.*
