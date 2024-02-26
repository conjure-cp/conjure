rm -rf conjure-output *.solution
conjure solve *.essence --solver kissat --validate-solutions
rm -rf conjure-output *.solution
