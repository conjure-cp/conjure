
rm -rf conjure-output

conjure solve riddle1.essence
conjure solve riddle1.essence --solver lingeling

conjure solve riddle2.essence
conjure solve riddle2.essence --solver lingeling

rm -rf conjure-output *.solution
