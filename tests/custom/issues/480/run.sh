rm -rf conjure-output *.solution model-*.*
conjure parameter-generator model.essence
conjure solve model-instanceGenerator.essence p.param
cat *.solution *.irace conjure-output/*.eprime | grep -v '^\$'
rm -rf conjure-output *.solution model-*.*
