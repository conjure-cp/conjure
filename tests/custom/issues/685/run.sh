rm -rf conjure-output *.solution
conjure solve 685.essence --savilerow-options -O0
cat *.solution
rm -rf conjure-output *.solution
conjure solve 685.essence --savilerow-options -O2
cat *.solution
rm -rf conjure-output *.solution
