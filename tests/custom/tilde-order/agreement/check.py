"""Dot order needs represented operands; the bare-constant validator cannot check it."""
import itertools
import json
import os
from pathlib import Path
import subprocess
import tempfile


def canonical(value):
    if isinstance(value, dict):
        return tuple(canonical(value[k]) for k in sorted(value, key=int))
    return tuple(canonical(v) for v in value) if isinstance(value, list) else value


def check(name, values, key):
    expected = {(a, b, key(a) < key(b), key(a) <= key(b))
                for a in values for b in values}
    with tempfile.TemporaryDirectory(prefix="conjure-order-agreement-") as output:
        result = subprocess.run([
            os.environ.get("CONJURE", "conjure"), "solve",
            str(Path(__file__).with_name(name + ".essence")),
            "--representations-finds=c", "--representation-levels=no",
            "--channelling=no", "--number-of-solutions=all",
            "--solutions-in-one-file", "--output-format=jsonstream",
            "--copy-solutions=no", "--output-directory=" + output,
        ], capture_output=True, text=True, timeout=120)
        assert result.returncode == 0, result.stdout + result.stderr
        models = list(Path(output).glob("*.eprime"))
        assert len(models) == 1, models
        rows = [json.loads(line) for line in
                (Path(output) / (models[0].stem + ".solutions.json")).read_text().splitlines()]
        actual = set()
        for row in rows:
            a, b = (canonical(row["pairs"]) if "pairs" in row else (row["a"], row["b"]))
            assert (row["dotlt"], row["dotleq"]) == (row["lt"], row["leq"]), row
            actual.add((canonical(a), canonical(b), row["lt"], row["leq"]))
        assert actual == expected and len(rows) == len(expected), (name, expected - actual, actual - expected)
    print(f"{name}: dot and global orders agree on {len(expected)} ordered pairs")


check("occurrence", [(), (1,), (2,)], lambda a: tuple(a.count(i) for i in (1, 2)))
# {{i}} is in increasing global order as i increases: two set layers reverse twice.
inner = [((i,),) for i in (1, 2, 3)]
check("deep", list(itertools.combinations(inner, 2)),
      lambda a: tuple(a.count(i) for i in inner))
check("tuple", list(itertools.product((1, 2), [(1,), (2,)])),
      lambda a: (a[0], tuple(a[1].count(i) for i in (1, 2))))
