rm -rf conjure-output *.solution

conjure solve 401-1.essence
cat conjure-output/model000001.eprime | grep -v "^[$]"
cat 401-1.solution

echo " ===================="

conjure solve 401-2.essence
cat conjure-output/model000001.eprime | grep -v "^[$]"
cat 401-2.solution

echo " ===================="

conjure solve 401-3.essence
cat conjure-output/model000001.eprime | grep -v "^[$]"
cat 401-3.solution

rm -rf conjure-output *.solution
