
rm -rf conjure-output* *.solution
conjure solve --number-of-solutions=10000 --unnamed-symmetry-breaking=Complete-Consecutive-Altogether unnamed-set-of-set.essence -o conjure-output-complete
cat conjure-output-complete/*.eprime | grep -v "^[$]"
rm -rf conjure-output-* *.solution
conjure solve --number-of-solutions=10000 --unnamed-symmetry-breaking=Quick-Consecutive-Altogether unnamed-set-of-set.essence -o conjure-output-quick
cat conjure-output-quick/*.eprime | grep -v "^[$]"
rm -rf conjure-output-* *.solution
