#!/bin/bash
set -euo pipefail
export LC_ALL=C

work_dir=$(mktemp -d)
trap 'rm -rf -- "$work_dir"' EXIT

for spec in *.essence; do
    stem=${spec%.essence}
    out="$work_dir/$stem"
    if ! conjure solve "$spec" -o "$out" \
        --unnamed-symmetry-breaking=none --number-of-solutions=all \
        --solutions-in-one-file --output-format=jsonstream --line-width=1000000 \
        --validate-solutions --copy-solutions=no > "$work_dir/solve.log" 2>&1; then
        cat "$work_dir/solve.log" >&2
        exit 1
    fi
    LC_ALL=C sort "$out"/*.solutions.json > "$out/sorted.json"
    diff -u "$stem.solutions.expected" "$out/sorted.json"
    printf '%s\n' "$stem"
done
