
rm -rf conjure-output
conjure modelling -ac model.essence
cat conjure-output/*.eprime | grep -v "^[$]" | head -n80
rm -rf conjure-output

echo ""
echo ""

conjure parameter-generator barman.essence --MAXINT=30
cat barman-instance*.essence*
rm -f barman-instance*.essence*

