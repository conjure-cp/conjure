# `.<` / `.<=` can compare two sides in different representations

Branch: `no-merge/dotleq-representation-mismatch` (not for merge)

## The problem

`rule_DotLtLeq` (`src/Conjure/UI/Model.hs`) refines `.<` / `.<=` like this:

```haskell
ma <- symmetryOrdering a >>= ...
mb <- symmetryOrdering b >>= ...
return $ mk ma mb          -- ma <=lex mb
```

`symmetryOrdering` is defined per **representation** — there are 23 implementations under
`src/Conjure/Representations/`. Each defines a perfectly good total order on the abstract
value, but different representations produce different, mutually incomparable encodings.
For a partial function on 3 points:

| representation | key | length |
| --- | --- | --- |
| `Function1DPartial` | `[ (-toInt(flags[i]), values[i]) \| i : indexDom ]` | 6 |
| `FunctionNDPartialDummy` | the dummy matrix, `0` = undefined | 3 |

Comparing one against the other lexicographically is meaningless. `rule_Eq` and
`rule_Neq` guard against exactly this with `sameRepresentationTree`. `rule_DotLtLeq`
never had that guard, and nothing else enforces the invariant either.

Channelling is what makes a mismatch reachable: a variable may carry more than one
representation, and each *occurrence* is assigned one independently.

## This is not only a symmetry-breaking bug

It reproduces with no symmetry breaking at all:

```essence
letting e be new type of size 3
find a : function e --> e
find b : function e --> e
such that a .<= b
```

`.<=` is a total order, so exactly `64*65/2 = 2080` of the `64*64` pairs must satisfy it,
in every model. Across `conjure modelling -ax` (16 models):

| both sides | solutions |
| --- | --- |
| `Function1DPartial` | 2080 (correct) |
| `FunctionNDPartialDummy` | 2080 (correct) |
| `Function1DPartial` vs `Dummy` | 4028 |
| `Dummy` vs `Function1DPartial` | 68 |

The generated constraint in a mixed model reads:

```
x_FunctionNDPartialDummy                                     <=lex     $ 3 ints, 0 = undefined
flatten([[-toInt(aux_Flags[..])],[aux_Values[..]] | q20])              $ 6 ints, (flag,value)
```

## Why symmetry breaking triggers it reliably

`Complete` emits `x .<= transform(p, x)`. The right-hand side becomes an auxiliary
(`LocalFind`), and auxiliaries get their representation from a *different* rule
(`rule_ChooseReprForLocals`), chosen independently of the `x` occurrence on the left. So
the two sides routinely disagree.

`Quick` is unaffected: `rule_QuickPermutationOrder` applies `symmetryOrdering` **once** and
transforms the resulting vector, so there is only ever one encoding in play.

Measured on `find x : function e --> e` (|e| = 3) with
`--unnamed-symmetry-breaking=Complete-AllPermutations-Independently`, over all 30 models:
12 of 30 are unsound — 5 return 1 solution where 16 orbits exist, 7 return 57 and lose the
empty function.

## What was tried

### 1. Guard only

Add `sameRepresentationTree a b` to `rule_DotLtLeq`, mirroring `rule_Eq`.

Mismatched models stop being generated (16 → 4 for `a .<= b`) and every surviving model is
correct. But conjure then dies with `bug` at `Model.hs:1048` ("Not refined: `a .<= b`") on
branches where selection had already committed to mismatched representations. The guard
makes the branch infeasible; nothing prunes it.

`=` tolerates a strict guard because horizontal rules (`Horizontal.Function.rule_Eq`, …)
decompose it structurally whatever the representations. `.<` / `.<=` has no such fallback —
`rule_DotLtLeq` is the only rule.

### 2. Auxiliary fallback (implemented, then rejected)

On mismatch, introduce an auxiliary in `a`'s representation, constrain `aux = b` (equality
across representations *is* handled), and compare `a .<= aux`.

Correct — 62 of 62 models returned 2080 — but it attaches an extra variable and channelling
constraints to every such comparison. That is the wrong trade for symmetry breaking, whose
whole purpose is to be cheap. Rejected on those grounds.

### 3. Constrain representation selection (what is on this branch)

Restrict the choice so the two operands agree, in both places representations are chosen:

- `rule_ChooseRepr` — top-level finds. It receives a `Zipper` "to query context" and was
  discarding it with `const`; the branch uses it to find the sibling operand.
- `rule_ChooseReprForLocals` — auxiliaries. The auxiliary's references are in `body`, so no
  zipper is needed; the body is scanned for a `.<` / `.<=` naming it.

Whichever side is decided second follows the first. No auxiliary, no extra constraints.

Results: `a .<= b` → 8 models, all 2080. `Complete-AllPermutations-Independently` → every
model 16. Two-variable `Altogether` → 45, matching Burnside `(81 + 3^2)/2`.

## Where this is unsatisfying: tuples

`representationTreeOf` fails for anything that is not a represented `Reference` or an
`OpIndexing`:

```haskell
representationTreeOf _ = failDoc "doesn't seem to have a representation"
```

So neither operand of `(i,i) .<= (j,j)`, nor of `(x,y,z) .<= transform(p,(x,y,z))` — which
is precisely what `Complete-…-Altogether` builds via `varsTuple` — has a representation
tree. Three consequences:

1. **A strict guard breaks tuples.** Adding `sameRepresentationTree` unconditionally failed
   22 custom tests (`dotlt/tuple/*`, `permutations/permInverse/06`–`19`, `symmetry/*`), every
   one of them "Not refined". These cases never had the bug — in `(i,i) .<= (j,j)` the
   components are plain ints and the comparison was always fine.

2. **So the guard has to be soft**, refusing only when *both* sides have a representation
   tree and the two differ. That restores all 22. But it means **tuple operands are not
   guarded at all** — they fall through to the same independent-`symmetryOrdering`
   comparison as before.

3. **The restriction does not reach inside tuples either.** The sibling lookup walks up one
   level from an occurrence; from a reference inside an `AbsLitTuple` it sees the tuple
   literal, not the enclosing `.<=`, so no alignment happens.

Tuple operands are therefore no worse than before this branch, but no better. Whether they
are *safe* depends on `rule_Tuple_DotLeq` always decomposing componentwise before
`rule_DotLtLeq` sees them, so that each component comparison presents represented
references. We have not established that it always does.

## Questions for the maintainers

1. Is "both sides of a `.<` / `.<=` share a representation" meant to be maintained by
   representation *selection*, or by the rule declining? At present neither happens.

2. Should `symmetryOrdering` be canonical **per abstract domain** rather than per
   representation? That removes the entire class of bug — every representation of a domain
   emits the same key — but it is 23 implementations, and some pairs (`Occurrence` vs
   `Explicit` sets) would have to emulate each other, possibly at real constraint cost.

3. For tuples, is `rule_Tuple_DotLeq` guaranteed to decompose before `rule_DotLtLeq` runs?
   If so the soft guard is sufficient and this is only a documentation matter. If not, the
   alignment needs to reach references nested inside tuple literals, matching component
   position across the two sides.

4. Is there an intended mechanism we have missed? `rule_Transform_DotLess_rest` exists but
   is never registered (GHC warns it is unused), and its body sketches a per-type treatment
   of `x .<= transform(ps, x)` for functions, sets, msets, relations and partitions.

## Aside: a brittle test

`tests/custom/permutations/permInverse/14` pins `--responses=1,1` … `4,4`. Any change to
the space of models shifts what those numbers select, so the test breaks — or worse, keeps
passing while exercising something else. The two tests added on this branch deliberately
assert invariants (a total order has `n(n+1)/2` pairs; complete symmetry breaking keeps one
representative per orbit) so that they neither depend on how many models are generated nor
on the order in which they are.
