# Permutations spec
Organized by level - each level adding functionality
At the moment a permutation's inner type is restricted
to types that can index a matrix i.e. int, enum, unnamed types

## 01 representation
Tests permutation behaviour that should work with no rewrite rules.
- permutations must parse correctly in model and parameter files
- the size attribute must constrain the size of the permutation
- enumeration tests for finding permutations
- TODO add tests for enums, unnameds

## 02 cardinality 
Test that we can get the number of permuted elements by |p|
- basic cardinality for find, letting, given
- TODO add tests for enums, unnameds

## 03 generators
Tests permutations in generator of a comprehension 
- TODO add tests for enums, unnameds

## 04 image of value of inner type under permutation
- TODO add tests for enums & unnameds

## 05 equality
Tests equality on permutations 
- basic equality for find, letting, given
- tests [|p| = i | p <- sp] where sp is a set of permutations

## 06 inverse
Testing inverse in minimal contexts

## 07 Compose
Testing composition in minimal contexts
