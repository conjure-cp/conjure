rm -rf conjure-output *.stats.json

for solver in minion kissat; do
    conjure solve model.essence \
        --solver="$solver" \
        --number-of-solutions=all \
        --print-solutions=no \
        --copy-solutions=no > /dev/null
    echo "$solver: $(grep '^SolverSolutionsFound:' conjure-output/*.eprime-info)"
    find conjure-output -type f -name '*solution*'
    rm -rf conjure-output *.stats.json
done
