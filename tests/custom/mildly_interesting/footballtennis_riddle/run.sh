
rm -rf conjure-output

conjure solve riddle1.essence
conjure solve riddle1.essence --solver glucose

conjure solve riddle2.essence
conjure solve riddle2.essence --solver glucose

rm -rf conjure-output *.solution
