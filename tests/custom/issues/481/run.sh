rm -rf conjure-output
conjure modelling -ac model.essence
cat conjure-output/*.eprime | grep -v '^\$' | head -n80
rm -rf conjure-output
