rm -rf conjure-output *.solution

conjure solve 443-workaround.essence
head -n6 conjure-output/model000001.eprime

conjure solve 443.essence
head -n6 conjure-output/model000001.eprime

conjure solve 443-bool.essence
head -n6 conjure-output/model000001.eprime
cat 443-bool.solution

conjure solve 443-bool-typed.essence
head -n6 conjure-output/model000001.eprime
cat 443-bool-typed.solution

rm -rf conjure-output *.solution
