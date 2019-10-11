
echo " ========== 1 =========="
rm -rf conjure-output
conjure modelling -ac 383-1.essence 2>&1
head -n30 conjure-output/model000001.eprime

echo ""
echo ""
echo " ========== 2 =========="
rm -rf conjure-output
conjure modelling -ac 383-2.essence 2>&1
head -n30 conjure-output/model000001.eprime

echo ""
echo ""
echo " ========== 3 =========="
rm -rf conjure-output
conjure modelling -ac 383-3.essence 2>&1


rm -rf conjure-output
