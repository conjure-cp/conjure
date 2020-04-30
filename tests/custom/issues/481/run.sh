
rm -rf conjure-output
conjure modelling -ac model.essence
cat conjure-output/*.eprime | grep -v '^\$' | head -n80
rm -rf conjure-output

echo ""
echo ""

conjure parameter-generator barman.essence --essence-out generator.essence --MAXINT=30
cat generator.essence*
rm -f generator.essence*

