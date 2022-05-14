rm -rf conjure-output *.solution
conjure solve -ac 448-1.essence ; cat conjure-output/model000001.eprime | grep -v "^[$]" ; cat 448-1.solution
conjure solve -ac 448-2.essence ; cat conjure-output/model000001.eprime | grep -v "^[$]" ; cat 448-2.solution
conjure solve -ac 448-3.essence ; cat conjure-output/model000001.eprime | grep -v "^[$]" ; cat 448-3.solution
rm -rf conjure-output *.solution
