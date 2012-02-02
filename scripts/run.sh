clear

# conjure-repr rules/repr/Set.Gent.repr "$1.essence"
# conjure-repr rules/repr/Set.Occurrence.repr "$1.essence"
# conjure-repr rules/repr/Set.Explicit.repr "$1.essence"
# conjure-repr "rules/repr/Set.Explicit(IntOnly).repr" "$1.essence"
# conjure-repr rules/repr/Set.ExplicitVarSize.repr "$1.essence"
# conjure-repr rules/repr/Set.Occurrence.repr rules/repr/Set.Explicit*.repr "$1.essence"
# conjure-repr rules/repr/*Matrix1D* "$1.essence"
conjure-repr rules/repr/* "$1.essence"


ls $1-repr/*.essence | parallel -k -j3 "conjure-refn rules/refn/* {}"

# moves 1-level-deep leafs up to cwd, and removes those *.essence files in thw cwd
# rm *.essence; find . -name "*.essence" -type f | parallel -j1 mv {} {//}-{/}; find . -type d | parallel -j1 rm -r {}
# rm *.essence; find . -name "*.essence" -type f | parallel -j1 mv {} {//}-{/}; find . -type d | parallel -j1 rm -r {}; ls *.essence | parallel -j1 mv {} {.}.eprime


# runs every *.essence file through savilerow
# ls -1 *.essence | parallel -k -j3 "echo {}; runsavilerow {} -runsolver"

# same above, but nohup.
# ls -1 *.essence | nohup parallel --noswap -j5 -k "echo {}; runsavilerow {} -runsolver" > nohup.out 2> nohup.err &
