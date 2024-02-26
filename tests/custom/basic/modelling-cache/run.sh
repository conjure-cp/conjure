rm -rf conjure-output *.solution
conjure modelling example.essence -aai --responses 1,2
ls -A conjure-output
conjure solve example.essence 2.param -aai --responses 1,2
cat *.solution
rm -rf conjure-output *.solution
