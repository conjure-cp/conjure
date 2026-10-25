rm -rf out-chain out-bools *.solution *.stats.json
conjure solve chain.essence --number-of-solutions=all --copy-solutions=no --output-directory=out-chain
echo '--- chain eprime ---'
grep -v '^\$' out-chain/model000001.eprime
echo '--- chain solutions ---'
grep -H '^letting' out-chain/*.solution | LC_ALL=C sort
conjure solve bools.essence --number-of-solutions=all --copy-solutions=no --output-directory=out-bools
echo '--- bools eprime ---'
grep -v '^\$' out-bools/model000001.eprime
echo '--- bools solutions ---'
grep -H '^letting' out-bools/*.solution | LC_ALL=C sort
rm -rf out-chain out-bools *.solution *.stats.json
