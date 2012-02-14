echo "[RUNNING] $@"

# clear

# conjure-repr rules/repr/Set.Gent.repr "$1.essence"
# conjure-repr rules/repr/Set.Occurrence.repr "$1.essence"
# conjure-repr rules/repr/Set.Explicit*.repr "$1.essence"
# conjure-repr "rules/repr/Set.Explicit(IntOnly).repr" "$1.essence"
# conjure-repr rules/repr/Set.ExplicitVarSize.repr "$1.essence"
# conjure-repr rules/repr/Set.Occurrence.repr rules/repr/Set.Explicit*.repr rules/repr/Function*.repr rules/repr/Relation*.repr "$1.essence"
# conjure-repr rules/repr/Set.*.repr rules/repr/*Matrix1D* "$1.essence" # +RTS -s

# conjure-repr rules/repr/*Matrix* rules/repr/Set* rules/repr/Relation* "$1.essence" +RTS -s

conjure-repr rules/repr/* "$1.essence" # +RTS -s

if (ls $1-repr/*.essence > /dev/null 2> /dev/null ) then

    echo "ls $1-repr/*.essence | parallel -j3 conjure-refn rules/refn/* {}"
    ls $1-repr/*.essence | parallel -j3 "conjure-refn rules/refn/* {}"

    # echo "rm -f $1-repr/*.essence"
    # rm -f $1-repr/*.essence

    echo "remove identical"
    # remove those output files which are identical to the input.
    if ( ls $1-repr/*-refn/*.essence > /dev/null 2> /dev/null ) then
        ls $1-repr/*-refn/*.essence | parallel "essence-diff $1.essence {} --rm"
    fi

    echo "move files to new dir"
    # create a new dir for the results.
    mkdir -p "$1"

    # move results of the refn phases to the new dir.
    if ( ls $1-repr/*-refn/*.essence > /dev/null 2> /dev/null ) then
        ls $1-repr/*-refn/*.essence | parallel -j1 "cp {} $1/{#}.essence"
    fi

    # remove the temp dir.
    # rm -r "$1-repr"

    # keep running
    echo "recurse."
    # ls $1/*.essence
    if ( ls $1/*.essence > /dev/null 2> /dev/null ) then
        # ls $1/*.essence | parallel --dry-run -j1 "sh ./$0 {.}"
        ls $1/*.essence | parallel -j1 "sh ./$0 {.}"
    fi

fi
