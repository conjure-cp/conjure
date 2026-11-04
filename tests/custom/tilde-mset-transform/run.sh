#!/bin/bash
set -euo pipefail
export LC_ALL=C

work_dir=$(mktemp -d)
trap 'rm -rf -- "$work_dir"' EXIT
for spec in *.essence; do
    stem=${spec%.essence}
    model_dir="$work_dir/$stem-model"
    if ! conjure modelling -ac "$spec" -o "$model_dir" \
        --unnamed-symmetry-breaking=none > "$work_dir/compile.log" 2>&1; then
        cat "$work_dir/compile.log" >&2
        exit 1
    fi
    params=("$stem"-*.param)
    [[ -f "${params[0]}" ]] || params=("")
    for param in "${params[@]}"; do
        args=("$spec")
        label=$stem
        if [[ -n "$param" ]]; then
            args+=("$param")
            label=${param%.param}
        fi
        out="$work_dir/$label"
        mkdir -p "$out"
        cp "$model_dir/model000001.eprime" "$out/"
        if ! conjure solve "${args[@]}" -o "$out" \
            --unnamed-symmetry-breaking=none --use-existing-models=model000001.eprime \
            --number-of-solutions=all \
            --solutions-in-one-file --output-format=jsonstream --line-width=1000000 \
            --validate-solutions --copy-solutions=no > "$work_dir/solve.log" 2>&1; then
            cat "$work_dir/solve.log" >&2
            exit 1
        fi
        sort "$out"/*.solutions.json > "$out/sorted.json"
        diff -u "$label.solutions.expected" "$out/sorted.json"
        printf '%s\n' "$label"
    done
done
