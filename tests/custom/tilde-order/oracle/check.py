"""Check complete truth tables, independently of Conjure's tilde evaluator."""

import itertools
import json
import os
from pathlib import Path
import subprocess
import tempfile


def check(kind, universe=(1, 2), other_universe=None, sizes=(2, 2),
          repeated=False):
    # Canonical semantic values, including empty collections and repetitions.
    combinations = (itertools.combinations_with_replacement if repeated
                    else itertools.combinations)

    def values(elements, maximum):
        return [value for size in range(maximum + 1)
                for value in combinations(elements, size)]

    other_universe = universe if other_universe is None else other_universe
    coordinates = sorted(set(universe) | set(other_universe))

    def frequencies(value):
        return tuple(value.count(i) for i in coordinates)

    def canonical(value):
        return tuple(sorted(tuple(v) if isinstance(v, list) else v for v in value))

    expected = {
        (a, b, frequencies(a) < frequencies(b), frequencies(a) <= frequencies(b))
        for a in values(universe, sizes[0])
        for b in values(other_universe, sizes[1])
    }
    with tempfile.TemporaryDirectory(prefix="conjure-tilde-") as temporary:
        output = Path(temporary)
        result = subprocess.run([
            os.environ.get("CONJURE", "conjure"), "solve",
            str(Path(__file__).with_name(kind + ".essence")),
            "--representations-finds=x", "--representation-levels=no",
            "--channelling=no", "--number-of-solutions=all",
            "--solutions-in-one-file", "--output-format=jsonstream",
            "--copy-solutions=no", "--output-directory=" + str(output),
        ], capture_output=True, text=True, timeout=300)
        if result.returncode:
            raise AssertionError(result.stdout + result.stderr)
        models = sorted(output.glob("*.eprime"))
        assert models, "No models generated"
        assert len(models) > 1, "Expected multiple representations"
        for model in models:
            solutions = output / (model.stem + ".solutions.json")
            assert solutions.exists(), (model.name, result.stdout, result.stderr)
            actual = set()
            for line in solutions.read_text().splitlines():
                row = json.loads(line)
                actual.add((canonical(row["a"]), canonical(row["b"]),
                            row["lt"], row["leq"]))
            assert actual == expected, (model.name, "missing", expected - actual,
                                        "extra", actual - expected)
    print(f"{kind}: all representations agree on {len(expected)} ordered pairs")


check("set")
check("mset", repeated=True)
check("relation", universe=((1, False), (1, True), (3, False), (3, True)))
check("set-different-domain", universe=(1, 3), other_universe=(2, 3))
check("mset-different-capacity", universe=(1, 3), sizes=(1, 2), repeated=True)
