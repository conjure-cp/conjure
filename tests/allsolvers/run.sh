
echo "default, minion"
conjure solve test.essence


# CP solvers

echo "\nminion"
conjure solve test.essence --solver minion

echo "\ngecode"
conjure solve test.essence --solver gecode

echo "\nchuffed"
conjure solve test.essence --solver chuffed


# SAT solvers

echo "\nglucose"
conjure solve test.essence --solver glucose

echo "\nglucose-syrup"
conjure solve test.essence --solver glucose-syrup

echo "\nlingeling"
conjure solve test.essence --solver lingeling

echo "\nplingeling"
conjure solve test.essence --solver plingeling

echo "\ntreengeling"
conjure solve test.essence --solver treengeling

echo "\ncadical"
conjure solve test.essence --solver cadical

echo "\nminisat"
conjure solve test.essence --solver minisat


# AllSAT solvers

echo "\nbc_minisat_all --number-of-solutions=all"
conjure solve test.essence --solver bc_minisat_all --number-of-solutions=all

echo "\nnbc_minisat_all --number-of-solutions=all"
conjure solve test.essence --solver nbc_minisat_all --number-of-solutions=all


# MaxSAT solvers

echo "\nopen-wbo"
conjure solve testo.essence --solver open-wbo


# MIP solvers (via MiniZinc)

echo "\ncoin-or"
conjure solve test.essence --solver coin-or

echo "\ncplex"
conjure solve test.essence --solver cplex


# SMT solvers

echo "\nboolector"
conjure solve test.essence --solver boolector

echo "\nboolector-bv"
conjure solve test.essence --solver boolector-bv

echo "\nyices"
conjure solve test.essence --solver yices

echo "\nyices-bv"
conjure solve test.essence --solver yices-bv

echo "\nyices-lia"
conjure solve test.essence --solver yices-lia

echo "\nyices-idl"
conjure solve test.essence --solver yices-idl

echo "\nz3"
conjure solve test.essence --solver z3

echo "\nz3-bv"
conjure solve test.essence --solver z3-bv

echo "\nz3-lia"
conjure solve test.essence --solver z3-lia

echo "\nz3-nia"
conjure solve test.essence --solver z3-nia

echo "\nz3-idl"
conjure solve test.essence --solver z3-idl


# remove the generated files
rm -rf conjure-output *.solution
