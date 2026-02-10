
rm -rf conjure-output *.solution *.stats.json

GRAPH="zkc"

conjure solve motif.essence data/$GRAPH.param --solver minion --output-format=json --copy-solutions=no
python3 readConjureSolution.py conjure-output/model000001-$GRAPH-solution000001.solution.json

rm -rf conjure-output *.solution *.stats.json
