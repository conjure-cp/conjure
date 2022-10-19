
rm -rf conjure-output

GRAPH="zkc"

conjure solve motif.essence data/$GRAPH.param --solver chuffed --output-format=json --copy-solutions=no
python3 readConjureSolution.py conjure-output/model000001-$GRAPH-solution000001.solution.json > data/$GRAPH.solution
cp conjure-output/model000001-$GRAPH.eprime-info data/$GRAPH.info
cat data/$GRAPH.solution

rm -rf conjure-output
