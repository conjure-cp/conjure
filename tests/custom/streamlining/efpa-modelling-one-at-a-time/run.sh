#!/usr/bin/env bash

rm -rf conjure-output-* results

n=$({ conjure streamlining EFPA.essence > /dev/null; } 2>&1 | sed -n 's/^Number of streamliners: *//p')

parallel -j2 --no-notice --halt now,fail=1 --results results 'conjure modelling -ac EFPA.essence --generate-streamliners="{}" -o conjure-output-{}' ::: $(seq 1 "$n")
status=$?

rm -rf conjure-output-* results
exit $status
