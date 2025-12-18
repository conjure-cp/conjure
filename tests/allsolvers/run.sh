#!/bin/bash

# get the script directory
DIR="$( cd "$( dirname "$0" )" && pwd )"

(
cd $DIR

rm -rf conjure-output

echo "default, minion"
conjure solve test.essence
echo ""
echo ""
echo "========================================"


# CP solvers

echo "minion"
conjure solve test.essence --solver minion
echo ""
echo ""
echo "========================================"

# echo "gecode"
# conjure solve test.essence --solver gecode
# echo ""
# echo ""
# echo "========================================"

echo "chuffed"
conjure solve test.essence --solver chuffed
echo ""
echo ""
echo "========================================"

echo "or-tools"
conjure solve test.essence --solver or-tools
echo ""
echo ""
echo "========================================"

# SAT solvers

echo "glucose"
conjure solve test.essence --solver glucose
echo ""
echo ""
echo "========================================"

echo "glucose-syrup"
conjure solve test.essence --solver glucose-syrup
echo ""
echo ""
echo "========================================"

echo "lingeling"
conjure solve test.essence --solver lingeling
echo ""
echo ""
echo "========================================"

echo "plingeling"
conjure solve test.essence --solver plingeling
echo ""
echo ""
echo "========================================"

echo "treengeling"
conjure solve test.essence --solver treengeling
echo ""
echo ""
echo "========================================"

echo "cadical"
conjure solve test.essence --solver cadical
echo ""
echo ""
echo "========================================"

# commenting out as we do not have an install script for it
# echo "minisat"
# conjure solve test.essence --solver minisat
# echo ""
# echo ""
# echo "========================================"

# AllSAT solvers

echo "bc_minisat_all --number-of-solutions=all"
conjure solve test.essence --solver bc_minisat_all --number-of-solutions=all
echo ""
echo ""
echo "========================================"

echo "nbc_minisat_all --number-of-solutions=all"
conjure solve test.essence --solver nbc_minisat_all --number-of-solutions=all
echo ""
echo ""
echo "========================================"


# MaxSAT solvers

# echo "wmaxcdcl"
# conjure solve testo.essence --solver wmaxcdcl
# echo ""
# echo ""
# echo "========================================"


# MIP solvers (via MiniZinc)

# commenting out as we do not have an install script for it
# echo "coin-or"
# conjure solve test.essence --solver coin-or
# echo ""
# echo ""
# echo "========================================"

# commenting out as we do not have an install script for it
# echo "cplex"
# conjure solve test.essence --solver cplex
# echo ""
# echo ""
# echo "========================================"


# SMT solvers

echo "boolector"
conjure solve test.essence --solver boolector
echo ""
echo ""
echo "========================================"

echo "boolector-bv"
conjure solve test.essence --solver boolector-bv
echo ""
echo ""
echo "========================================"

echo "yices"
conjure solve test.essence --solver yices
echo ""
echo ""
echo "========================================"

echo "yices-bv"
conjure solve test.essence --solver yices-bv
echo ""
echo ""
echo "========================================"

echo "yices-lia"
conjure solve test.essence --solver yices-lia
echo ""
echo ""
echo "========================================"

echo "yices-idl"
conjure solve test.essence --solver yices-idl
echo ""
echo ""
echo "========================================"

echo "z3"
conjure solve test.essence --solver z3
echo ""
echo ""
echo "========================================"

echo "z3-bv"
conjure solve test.essence --solver z3-bv
echo ""
echo ""
echo "========================================"

echo "z3-lia"
conjure solve test.essence --solver z3-lia
echo ""
echo ""
echo "========================================"

echo "z3-nia"
conjure solve test.essence --solver z3-nia
echo ""
echo ""
echo "========================================"

echo "z3-idl"
conjure solve test.essence --solver z3-idl
echo ""
echo ""
echo "========================================"


# remove the generated files
rm -rf conjure-output *.solution

)
