
rm -rf conjure-output* *.solution *.stats.json
conjure solve --number-of-solutions=10 --unnamed-symmetry-breaking=Eager-Consecutive-Altogether *.essence -o conjure-output-eager
cat conjure-output-eager/*.eprime | grep -v "^[$]"
rm -rf conjure-output-* *.solution *.stats.json
conjure solve --number-of-solutions=10 --unnamed-symmetry-breaking=Delayed-Consecutive-Altogether *.essence -o conjure-output-delayed
cat conjure-output-delayed/*.eprime | grep -v "^[$]"
rm -rf conjure-output-* *.solution *.stats.json
