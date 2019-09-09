rm -rf conjure-output *.solution
conjure solve -ac 448-1.essence ; head -n29 conjure-output/model000001.eprime ; cat 448-1.solution
conjure solve -ac 448-2.essence ; head -n15 conjure-output/model000001.eprime ; cat 448-2.solution
conjure solve -ac 448-3.essence ; head -n16 conjure-output/model000001.eprime ; cat 448-3.solution
rm -rf conjure-output *.solution
