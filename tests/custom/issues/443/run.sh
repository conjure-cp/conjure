rm -rf conjure-output *.solution

conjure solve 443-workaround.essence
cat conjure-output/model000001.eprime | grep -v "^[$]"

conjure solve 443.essence
cat conjure-output/model000001.eprime | grep -v "^[$]"

conjure solve 443-bool.essence
cat conjure-output/model000001.eprime | grep -v "^[$]"
cat 443-bool.solution

conjure solve 443-bool-typed.essence
cat conjure-output/model000001.eprime | grep -v "^[$]"
cat 443-bool-typed.solution

rm -rf conjure-output *.solution
