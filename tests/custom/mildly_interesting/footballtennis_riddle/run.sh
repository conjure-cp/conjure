
rm -rf conjure-output

conjure solve riddle1.essence
conjure solve riddle1.essence --solver kissat

conjure solve riddle2.essence
conjure solve riddle2.essence --solver kissat

rm -rf conjure-output *.solution
