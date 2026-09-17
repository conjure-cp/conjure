
rm -rf conjure-output* *.stats.json
conjure solve --solutions-in-one-file --number-of-solutions=all --output-format=jsonstream --unnamed-symmetry-breaking=Delayed-AllPermutations-Independently *.essence *.param -o conjure-output-Delayed-AllPermutations-Independently --copy-solutions=no
cat conjure-output-Delayed-AllPermutations-Independently/*.solutions.json
rm -rf conjure-output* *.stats.json

# conjure solve --solutions-in-one-file --number-of-solutions=all --output-format=jsonstream --unnamed-symmetry-breaking=Eager-AllPermutations-Independently *.essence *.param -o conjure-output-Eager-AllPermutations-Independently
