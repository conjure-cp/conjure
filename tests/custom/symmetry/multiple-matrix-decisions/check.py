import itertools
import json
import os
from pathlib import Path
import subprocess
import tempfile


def orbit(bits):
    a, b, c, d, p, q = bits
    return min(bits, (c, d, a, b, q, p))


def solve(model, out, mode):
    result = subprocess.run(
        [os.environ.get('CONJURE', 'conjure'), 'solve', str(model), '-o', str(out),
         '--unnamed-symmetry-breaking=' + mode, '--number-of-solutions=all',
         '--solutions-in-one-file', '--output-format=jsonstream', '--validate-solutions'],
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, timeout=60)
    assert result.returncode == 0, result.stdout
    files = list(out.glob('*.solutions.json'))
    assert len(files) == 1, result.stdout
    # Decode consecutive JSON values: jsonstream can wrap a record across lines.
    decoder = json.JSONDecoder()
    remaining = files[0].read_text().strip()
    solutions = []
    while remaining:
        value, end = decoder.raw_decode(remaining)
        solutions.append(value)
        remaining = remaining[end:].strip()
    return solutions


all_bits = set(itertools.product(range(2), repeat=6))
expected = {orbit(bits) for bits in all_bits}
assert len(all_bits) == 64 and len(expected) == 36
modes = ['none'] + ['-'.join(parts) for parts in itertools.product(
    ['Delayed', 'Eager'], ['Consecutive', 'AllPairs', 'AllPermutations'],
    ['Independently', 'Altogether'])]
with tempfile.TemporaryDirectory(prefix='conjure-multiple-matrices-') as tmp:
    root = Path(tmp)
    model = root / 'model.essence'
    model.write_text(Path('model.essence').read_text())
    for mode in modes:
        solutions = solve(model, root / mode, mode)
        bits = [tuple(s['design'][i][j] for i in ['1', '2'] for j in ['1', '2'])
                + tuple(s['printings'][i] for i in ['1', '2']) for s in solutions]
        assert len(bits) == len(set(bits)), mode
        assert set(bits) <= all_bits, mode
        assert {orbit(b) for b in bits} == expected, mode
        assert len(bits) == (64 if mode == 'none' else 36), (mode, len(bits))
        print(f'{mode}: {len(bits)} solutions, all 36 orbits preserved', flush=True)
    equality = root / 'equality.essence'
    equality.write_text(Path('equality.essence').read_text())
    solutions = solve(equality, root / 'equality', 'none')
    actual = set()
    for solution in solutions:
        a, b, c = (solution['x'][str(i)] for i in range(1, 4))
        checks = tuple(solution['checks'][str(i)] for i in range(1, 11))
        expected_checks = (a == b == c, not (a == b == c), a == b, False, True,
                           a == b == c, a == b == c, a == b, not (a == b == c), True)
        assert checks == expected_checks, (solution, expected_checks)
        actual.add((a, b, c))
    assert len(solutions) == 8 and actual == set(itertools.product(range(2), repeat=3))
    print('comprehension equality: all 8 assignments verified', flush=True)
