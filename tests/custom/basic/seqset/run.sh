rm -rf conjure-output *.solution *.stats.json
conjure solve *.essence --solver kissat --validate-solutions
rm -rf conjure-output *.solution *.stats.json
