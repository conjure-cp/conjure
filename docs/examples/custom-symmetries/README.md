# Custom symmetry breaking for experiments

`applySymmetries(values, symmetries)` posts Complete symmetry breaking for a supplied list. `applySymmetriesQuick(values, symmetries)` uses Conjure's existing Quick method instead. Both use the existing representation-specific order, not `~<=`.

Run this example with:

```sh
conjure solve model.essence swap.param -ac \
  --unnamed-symmetry-breaking=none --number-of-solutions=all \
  --solutions-in-one-file --validate-solutions
```

The constraint fixes `f(1:E) = 1:E`. Only the identity and the swap of 2 and 3 are supplied: the full symmetric group would not preserve this problem. Complete keeps one representative from each of the six orbits. Change the operator name to compare Quick with Complete using the same parameter file.

## Input contract

- Put each operator directly in `such that`, as a separate assertion. These are symmetry-breaking hints; they cannot be negated, reified, used inside another expression, or used to define a variable.
- `values` is a tuple of variable references (aliases and nested tuple literals are allowed). Use `tuple(f)` for a singleton. Tuple order controls comparison priority.
- `symmetries` is a constant or given one-dimensional integer-indexed matrix of permutation tuples. Each tuple is one joint action on all the selected values. Its positions identify type actions, not individual variables.
- Permutations in one tuple must target distinct types. Use tags to distinguish independent actions on integer domains. For example, `tuple(permutation of int:A(1..2), permutation of int:B(1..3))` permits different domain sizes and acts on A and B independently.
- Permutation literals omit fixed points. The declared parameter domain supplies the type of an empty identity permutation.
- Use one common ordered value tuple for all symmetry assertions in a model. Independently minimizing different tuples can remove whole orbits even when each supplied action is a valid symmetry.
- Complete orders the selected tuple; omitted variables can leave multiple full solutions with the same selected tuple.
- The supplied entries are applied exactly as given. Conjure does not generate group closure. Empty lists impose no constraints; identity or repeated entries do not strengthen the result.
- Every supplied action must be a symmetry of the entire problem. Conjure checks types and parameter validity, but cannot prove this semantic property for arbitrary constraints. Supplying generators alone need not produce complete symmetry breaking.
- Disable automatic unnamed symmetry breaking for custom-group experiments, as above. Otherwise the automatic constraints are additional constraints and may use symmetries that your problem does not have.

## Implementation and correctness

Automatic and custom Complete symmetry breaking share a dedicated internal operation. Once the original representation is selected, its transformed auxiliary is created with exactly the same representation tree, including nested components. Both keys use the existing `symmetryOrdering` methods and are compared lexicographically. Scalars and primitive matrices can be transformed directly, using inverse permutation of matrix indices, without an auxiliary. A different auxiliary representation preference cannot silently change one side's key.

All symmetry assertions share the selected source representation for a variable, while ordinary constraints may still use channelling. Quick transforms the original's ordering key using the existing machinery. Actions on distinct tagged types are applied sequentially; tuple positions remain fixed. It can retain more representatives than Complete. Neither mode changes the general meaning or implementation of ordinary user-written `.<=` comparisons; the known general dot-order issues remain outside this paper-focused change.

Abstract solution validation checks the problem's ordinary constraints and domains, excluding these top-level representation-dependent hints. Their correctness is checked separately by normal regression tests that enumerate original solutions, independently canonicalize their orbits, and verify both modes preserve all orbits across the tested representations. In these tests the selected tuple contains every decision variable, so Complete with all group elements supplied must keep one abstract representative per orbit. Refinement examples are checked separately as well.
