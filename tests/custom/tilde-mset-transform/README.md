# Direct transformed multiset refinement

The cases retain direct `x ~<= transform(p,x)` constraints. They introduce no
image decision variable. Saved JSON answer sets were independently enumerated
using the recursive predefined frequency-vector order, then checked against
every validated solver assignment.

Coverage includes strict/nonstrict order, empty and repeated inner/outer
multisets, a three-cycle, quantified adjacent swaps through a decision tuple,
and symbolic outer/inner size bounds. Frequency tests check multiplicities of
empty and singleton entries in the permuted image.

Symbolic parameters use unrestricted integer declarations with separate where
bounds, avoiding the separately tracked parameter-tagging issue. JSON solution
output avoids the plain printer's loss of matrix index tags.
