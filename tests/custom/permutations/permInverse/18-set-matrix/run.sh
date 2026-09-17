
rm -rf conjure-output* *.stats.json
conjure solve --solutions-in-one-file --number-of-solutions=all --output-format=jsonstream --unnamed-symmetry-breaking=Delayed-AllPermutations-Independently *.essence *.param -o conjure-output-function-unnamed-Delayed-AllPermutations-Independently --copy-solutions=no
cat conjure-output-function-unnamed-Delayed-AllPermutations-Independently/*.solutions.json
rm -rf conjure-output* *.stats.json

# conjure solve --solutions-in-one-file --number-of-solutions=all --output-format=jsonstream --unnamed-symmetry-breaking=Eager-AllPermutations-Independently *.essence *.param -o conjure-output-function-unnamed-Eager-AllPermutations-Independently
