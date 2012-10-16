#!/bin/sh

scripts/run/conjure-repr.sh $1
ls $1-repr/*.essence   2> /dev/null | parallel "scripts/run/conjure-refn.sh {}"
ls $1-repr/*/*.essence 2> /dev/null | parallel "scripts/run/conjure-all-parallel.sh {}"

