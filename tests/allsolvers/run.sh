#!/bin/bash

# get the script directory
DIR="$( cd "$( dirname "$0" )" && pwd )"

(
cd $DIR

echo "default, minion"
rm -rf conjure-output
conjure solve test.essence
echo ""
echo ""
echo "========================================"


# CP solvers

echo "minion"
rm -rf conjure-output
conjure solve test.essence --solver minion
echo ""
echo ""
echo "========================================"

echo "gecode"
rm -rf conjure-output
conjure solve test.essence --solver gecode
echo ""
echo ""
echo "========================================"

echo "chuffed"
rm -rf conjure-output
conjure solve test.essence --solver chuffed
echo ""
echo ""
echo "========================================"

echo "or-tools"
rm -rf conjure-output
conjure solve test.essence --solver or-tools
echo ""
echo ""
echo "========================================"

# SAT solvers

echo "glucose"
rm -rf conjure-output
conjure solve test.essence --solver glucose
echo ""
echo ""
echo "========================================"

echo "glucose-syrup"
rm -rf conjure-output
conjure solve test.essence --solver glucose-syrup
echo ""
echo ""
echo "========================================"

echo "cadical"
rm -rf conjure-output
conjure solve test.essence --solver cadical
echo ""
echo ""
echo "========================================"

# commenting out as we do not have an install script for it
# echo "minisat"
# rm -rf conjure-output
# conjure solve test.essence --solver minisat
# echo ""
# echo ""
# echo "========================================"

# AllSAT solvers

echo "bc_minisat_all --number-of-solutions=all"
rm -rf conjure-output
conjure solve test.essence --solver bc_minisat_all --number-of-solutions=all
echo ""
echo ""
echo "========================================"

echo "nbc_minisat_all --number-of-solutions=all"
rm -rf conjure-output
conjure solve test.essence --solver nbc_minisat_all --number-of-solutions=all
echo ""
echo ""
echo "========================================"

echo "bdd_minisat_all --number-of-solutions=all"
rm -rf conjure-output
conjure solve test.essence --solver bdd_minisat_all --number-of-solutions=all
echo ""
echo ""
echo "========================================"

# MaxSAT solvers

echo "wmaxcdcl"
rm -rf conjure-output
conjure solve testo.essence --solver wmaxcdcl
echo ""
echo ""
echo "========================================"


# MIP solvers (via MiniZinc)

# commenting out as we do not have an install script for it
# echo "coin-or"
# rm -rf conjure-output
# conjure solve test.essence --solver coin-or
# echo ""
# echo ""
# echo "========================================"

# commenting out as we do not have an install script for it
# echo "cplex"
# rm -rf conjure-output
# conjure solve test.essence --solver cplex
# echo ""
# echo ""
# echo "========================================"


# SMT solvers

echo "boolector"
rm -rf conjure-output
conjure solve test.essence --solver boolector
echo ""
echo ""
echo "========================================"

echo "boolector-bv"
rm -rf conjure-output
conjure solve test.essence --solver boolector-bv
echo ""
echo ""
echo "========================================"

echo "yices"
rm -rf conjure-output
conjure solve test.essence --solver yices
echo ""
echo ""
echo "========================================"

echo "yices-bv"
rm -rf conjure-output
conjure solve test.essence --solver yices-bv
echo ""
echo ""
echo "========================================"

echo "yices-lia"
rm -rf conjure-output
conjure solve test.essence --solver yices-lia
echo ""
echo ""
echo "========================================"

echo "yices-idl"
rm -rf conjure-output
conjure solve test.essence --solver yices-idl
echo ""
echo ""
echo "========================================"

echo "z3"
rm -rf conjure-output
conjure solve test.essence --solver z3
echo ""
echo ""
echo "========================================"

echo "z3-bv"
rm -rf conjure-output
conjure solve test.essence --solver z3-bv
echo ""
echo ""
echo "========================================"

echo "z3-lia"
rm -rf conjure-output
conjure solve test.essence --solver z3-lia
echo ""
echo ""
echo "========================================"

echo "z3-nia"
rm -rf conjure-output
conjure solve test.essence --solver z3-nia
echo ""
echo ""
echo "========================================"

# remove the generated files
rm -rf conjure-output *.solution

)
