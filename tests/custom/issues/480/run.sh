rm -rf conjure-output *.solution generator.*
conjure parameter-generator model.essence --essence-out generator.essence
conjure solve generator.essence p.param
cat *.solution *.irace conjure-output/*.eprime | grep -v '^\$'
rm -rf conjure-output *.solution generator.*
