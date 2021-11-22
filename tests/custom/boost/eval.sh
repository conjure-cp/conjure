
# Something like:

conjure boost inp.essence > out.essence

X = chuffed/kissat/cplex

conjure solve inp.essence -o inp --solver X
conjure solve out.essence -o out --solver X

grep TotalTime inp/*info out/*info
grep Nodes inp/*info out/*info
