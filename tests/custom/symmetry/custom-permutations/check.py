"""Small exhaustive orbit checks, using the normal custom-test harness.

Enumerate the original model, then check every generated representation with
custom symmetry breaking. Canonicalization below does not use Conjure ordering.
"""
import itertools
import json
import os
from pathlib import Path
import subprocess
import tempfile

CONJURE = os.environ.get('CONJURE', 'conjure')


def run(args, log):
    p = subprocess.run([CONJURE, *map(str, args)], text=True,
                       stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=300)
    log.write_text(p.stdout)
    assert p.returncode == 0 and 'Error:' not in p.stdout and 'conjure: user error' not in p.stdout, p.stdout
    return p.stdout


def permutation(images, tag):
    seen, cycles = set(), []
    for i in range(1, len(images)+1):
        if i in seen:
            continue
        cycle, j = [], i
        while j not in seen:
            seen.add(j)
            cycle.append(f'{j}:{tag}')
            j = images[j-1]
        if len(cycle) > 1:
            cycles.append('(' + ','.join(cycle) + ')')
    # Explicit type on identity avoids losing the targeted domain.
    return 'permutation(' + ','.join(cycles) + ')'


def canonical(value, shape, action):
    if isinstance(shape, str):
        if shape == 'bool': return bool(value)
        v = int(str(value).split(':')[0])
        return action[shape][v-1] if shape in action else v
    if shape[0] == 'collection':
        return tuple(sorted(canonical(v, shape[1], action) for v in value))
    if shape[0] == 'function':
        return tuple(sorted((canonical(k, shape[1], action), canonical(v, shape[2], action))
                            for k, v in value.items()))
    raise AssertionError(shape)


def check_case(root, name, declarations, variables, groups, constraint='', all_reps=False):
    if os.environ.get('CASE_FILTER') and name not in os.environ['CASE_FILTER'].split(','):
        return
    d = root/name
    d.mkdir()
    actions = [dict(zip(groups, ps)) for ps in itertools.product(*groups.values())]
    def key(sol, action):
        return tuple(canonical(sol[n], shape, action) for n, shape in variables)
    def orbit(sol):
        return min(key(sol, p) for p in actions)
    types = {tag: len(ps[0]) for tag, ps in groups.items()}
    prefix = '\n'.join(f'letting {tag} be domain int:{tag}(1..{n})' for tag, n in types.items())+'\n'
    base = prefix+declarations+'\n'+('such that '+constraint+'\n' if constraint else '')
    param = d/'input.param'
    rows = ['tuple('+','.join(permutation(a[tag], tag) for tag in groups)+')' for a in actions]
    param.write_text('letting symmetries be ['+','.join(rows)+']\n')
    expected = None
    counts = []
    for mode in ['none', 'applySymmetries', 'applySymmetriesQuick']:
        model = d/(mode+'.essence')
        text = base
        if mode != 'none':
            dom = 'tuple('+','.join('permutation of '+tag for tag in groups)+')'
            text = prefix+f'given symmetries : matrix indexed by [int(1..{len(actions)})] of {dom}\n'+declarations+'\n'
            text += 'such that '+(constraint+',\n' if constraint else '')+mode+'(tuple('+','.join(n for n,_ in variables)+'), symmetries)\n'
        model.write_text(text)
        out = d/mode
        args = ['solve', model]
        if mode != 'none': args.append(param)
        args += ['-o', out, '-a', 'f', '--unnamed-symmetry-breaking=none', '--number-of-solutions=all',
                 '--solutions-in-one-file', '--output-format=jsonstream', '--validate-solutions',
                 '--representations-auxiliaries=s']
        if all_reps:
            args += ['--representations-finds=x', '--channelling=' + ('no' if name == 'set-of-sets' else 'yes')]
        run(args, d/(mode+'.log'))
        solution_files = sorted(out.glob('*.solutions.json'))
        assert solution_files, (name, mode)
        mode_counts = []
        for f in solution_files:
            decoder = json.JSONDecoder()
            remaining, solutions = f.read_text(), []
            while remaining.strip():
                v, end = decoder.raw_decode(remaining.lstrip())
                solutions.append(v)
                remaining = remaining.lstrip()[end:]
            actual = {orbit(s) for s in solutions}
            if mode == 'none':
                if expected is None: expected = actual
            assert actual == expected, (name, mode, f.name, len(expected), len(actual), expected-actual)
            if mode == 'applySymmetries':
                assert len(solutions) == len(expected), (name, f.name, len(solutions),len(expected))
            mode_counts.append(len(solutions))
        counts.append(mode_counts)
    print(f'{name}: {len(expected)} orbits preserved in both modes across all tested representations')
    return d


with tempfile.TemporaryDirectory(prefix='conjure-custom-symmetries-') as tmp:
    root = Path(tmp)
    s2 = list(itertools.permutations(range(1,3)))
    s3 = list(itertools.permutations(range(1,4)))
    function = ('function','E','E')
    check_case(root, 'total-function', 'find f : function (total) E --> E', [('f',function)], {'E':s3}, all_reps=True)
    check_case(root, 'partial-function', 'find f : function E --> E', [('f',function)], {'E':s3}, all_reps=True)
    check_case(root, 'proper-subgroup', 'find f : function (total) E --> E', [('f',function)],
               {'E':[(1,2,3),(1,3,2)]}, 'f(1:E) = 1:E', all_reps=True)
    check_case(root, 'set-of-sets', 'find x : set (size 2) of set (maxSize 2) of E',
               [('x',('collection',('collection','E')))], {'E':s3}, all_reps=True)
    check_case(root, 'mset-of-msets', 'find x : mset (size 2) of mset (size 2) of E',
               [('x',('collection',('collection','E')))], {'E':s2})
    check_case(root, 'set-of-functions', 'find x : set (size 2) of function (total) E --> E',
               [('x',('collection',function))], {'E':s2})
    check_case(root, 'tuple', 'find f : function E --> E\nfind s : set of E',
               [('f',function),('s',('collection','E'))], {'E':s2})
    check_case(root, 'cross-type-function', 'find f : function (total) E --> F',
               [('f',('function','E','F'))], {'E':s2,'F':s3}, all_reps=True)
    check_case(root, 'matrix-tuple', 'find x : matrix indexed by [int(1..2)] of E\nfind y : matrix indexed by [int(1..3)] of E',
               [('x',('function','index','E')),('y',('function','index','E'))], {'E':s3})
    check_case(root, 'boolean-matrix', 'find x : matrix indexed by [E] of bool',
               [('x',('function','E','bool'))], {'E':s3})
    check_case(root, 'two-types', 'find x : E\nfind y : F', [('x','E'),('y','F')], {'E':s2,'F':s3})

    # The same compiled model accepts empty, identity and duplicate lists.
    for mode in ['applySymmetries', 'applySymmetriesQuick']:
        d = root/('edges-'+mode);d.mkdir()
        model = d/'model.essence'
        model.write_text('letting E be domain int:E(1..3)\n'
                         'given k : int(0..)\n'
                         'given ps : matrix indexed by [int(1..k)] of tuple(permutation of E)\n'
                         'find x : E\n'
                         f'such that {mode}(tuple(x), ps)\n')
        for k in [0,1,2]:
            param = d/f'{k}.param'
            param.write_text(f'letting k be {k}\nletting ps be ['+','.join(['tuple(permutation())']*k)+']\n')
            out=d/str(k)
            run(['solve',model,param,'-ac','-o',out,'--unnamed-symmetry-breaking=none',
                 '--number-of-solutions=all','--solutions-in-one-file','--output-format=jsonstream',
                 '--validate-solutions'],d/f'{k}.log')
            sols=list(out.glob('*.solutions.json'))
            assert len(sols)==1
            assert len([json.loads(l) for l in sols[0].read_text().splitlines() if l.strip()])==3
        param = d/'generator.param'
        param.write_text('letting k be 1\nletting ps be [tuple(permutation((1:E,2:E,3:E)))]\n')
        out = d/'generator'
        run(['solve',model,param,'-ac','-o',out,'--unnamed-symmetry-breaking=none',
             '--number-of-solutions=all','--solutions-in-one-file','--output-format=jsonstream',
             '--validate-solutions'],d/'generator.log')
        sols=list(out.glob('*.solutions.json'))
        assert len(sols)==1
        assert len([json.loads(l) for l in sols[0].read_text().splitlines() if l.strip()])==2
    print('empty, identity and duplicate lists: unchanged solution sets in both modes')
    print('a single generator is applied without computing group closure')

    invalid = [
        ('nested', 'letting ps be [tuple(permutation((1:E,2:E)))]',
         '!applySymmetries(tuple(x), ps)', 'top-level such-that'),
        ('non-permutation', 'letting ps be [tuple(1)]',
         'applySymmetries(tuple(x), ps)', 'not a permutation'),
        ('duplicate-type', 'letting ps be [tuple(permutation((1:E,2:E)),permutation((1:E,2:E)))]',
         'applySymmetries(tuple(x), ps)', 'multiple permutations'),
        ('decision-list', 'find ps : matrix indexed by [int(1..1)] of tuple(permutation of E)',
         'applySymmetries(tuple(x), ps)', 'constant or given'),
        ('value-expression', 'letting ps be [tuple(permutation((1:E,2:E)))]',
         'applySymmetries(tuple(x+1:E), ps)', 'variable references'),
    ]
    for name, decl, assertion, message in invalid:
        model=root/(name+'.essence')
        model.write_text('letting E be domain int:E(1..2)\nfind x : E\n'+decl+'\nsuch that '+assertion+'\n')
        proc=subprocess.run([CONJURE,'modelling',str(model),'-ac','--unnamed-symmetry-breaking=none',
                             '-o',str(root/(name+'-out'))],text=True,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,timeout=30)
        assert message in proc.stdout, (name,proc.stdout)
    print('invalid custom assertions: rejected with diagnostics')

    # Compare the generated refinement itself, not whether a particular rule fired.
    fixtures = Path(__file__).resolve().parent
    for mode in ['applySymmetries', 'applySymmetriesQuick']:
        model=root/('refinement-'+mode+'.essence')
        model.write_text((fixtures/'refinement.essence').read_text().replace('applySymmetries(',mode+'('))
        out=root/('refinement-'+mode)
        run(['modelling',model,'-a','f','--unnamed-symmetry-breaking=none','-o',out],root/(mode+'-refinement.log'))
        actual=(out/'model000001.eprime').read_text().split("$ Conjure's",1)[0].rstrip()+'\n'
        assert actual == (fixtures/'expected'/(mode+'.eprime')).read_text(), mode+' refinement changed'
    print('Complete and Quick Essence-prime refinements match snapshots')
