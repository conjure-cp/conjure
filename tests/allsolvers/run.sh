
echo -e "default, minion"
conjure solve test.essence


# CP solvers

echo -e "\nminion"
conjure solve test.essence --solver minion

echo -e "\ngecode"
conjure solve test.essence --solver gecode

echo -e "\nchuffed"
conjure solve test.essence --solver chuffed


# SAT solvers

echo -e "\nglucose"
conjure solve test.essence --solver glucose

echo -e "\nglucose-syrup"
conjure solve test.essence --solver glucose-syrup

echo -e "\nlingeling"
conjure solve test.essence --solver lingeling

echo -e "\nplingeling"
conjure solve test.essence --solver plingeling

echo -e "\ntreengeling"
conjure solve test.essence --solver treengeling

echo -e "\ncadical"
conjure solve test.essence --solver cadical

echo -e "\nminisat"
conjure solve test.essence --solver minisat


# AllSAT solvers

echo -e "\nbc_minisat_all --number-of-solutions=all"
conjure solve test.essence --solver bc_minisat_all --number-of-solutions=all

echo -e "\nnbc_minisat_all --number-of-solutions=all"
conjure solve test.essence --solver nbc_minisat_all --number-of-solutions=all


# MaxSAT solvers

echo -e "\nopen-wbo"
conjure solve testo.essence --solver open-wbo


# MIP solvers (via MiniZinc)

echo -e "\ncoin-or"
conjure solve test.essence --solver coin-or

echo -e "\ncplex"
conjure solve test.essence --solver cplex


# SMT solvers

echo -e "\nboolector"
conjure solve test.essence --solver boolector

echo -e "\nboolector-bv"
conjure solve test.essence --solver boolector-bv

echo -e "\nyices"
conjure solve test.essence --solver yices

echo -e "\nyices-bv"
conjure solve test.essence --solver yices-bv

echo -e "\nyices-lia"
conjure solve test.essence --solver yices-lia

echo -e "\nyices-idl"
conjure solve test.essence --solver yices-idl

echo -e "\nz3"
conjure solve test.essence --solver z3

echo -e "\nz3-bv"
conjure solve test.essence --solver z3-bv

echo -e "\nz3-lia"
conjure solve test.essence --solver z3-lia

echo -e "\nz3-nia"
conjure solve test.essence --solver z3-nia

echo -e "\nz3-idl"
conjure solve test.essence --solver z3-idl


# remove the generated files
rm -rf conjure-output *.solution
