
rm -rf conjure-output *.stats.json

conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Delayed-AllPermutations-Independently

# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=full
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Delayed-Consecutive-Independently
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Delayed-Consecutive-Altogether
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Delayed-AllPairs-Independently
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Delayed-AllPairs-Altogether
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Delayed-AllPermutations-Independently
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Delayed-AllPermutations-Altogether
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Eager-Consecutive-Independently
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Eager-Consecutive-Altogether
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Eager-AllPairs-Independently
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Eager-AllPairs-Altogether
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Eager-AllPermutations-Independently
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Eager-AllPermutations-Altogether

cat conjure-output/model000001.solutions.json
rm -rf conjure-output *.stats.json
