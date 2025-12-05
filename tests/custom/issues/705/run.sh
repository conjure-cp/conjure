rm -rf conjure-output *.solution
conjure solve 1.essence --number-of-solutions=all
cat *.solution
rm -rf conjure-output *.solution
conjure solve 2.essence --number-of-solutions=all
cat *.solution
rm -rf conjure-output *.solution

