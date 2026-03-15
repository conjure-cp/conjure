
echo " ========== 1 =========="
rm -rf conjure-output *.solution *.stats.json
conjure modelling -ac 383-1.essence 2>&1
cat conjure-output/model000001.eprime | grep -v "^[$]"

echo ""
echo ""
echo " ========== 2 =========="
rm -rf conjure-output *.solution *.stats.json
conjure modelling -ac 383-2.essence 2>&1
cat conjure-output/model000001.eprime | grep -v "^[$]"

echo ""
echo ""
echo " ========== 3 =========="
rm -rf conjure-output *.solution *.stats.json
conjure modelling -ac 383-3.essence 2>&1


rm -rf conjure-output *.solution *.stats.json
