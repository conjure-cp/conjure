
rm -rf conjure-output*
conjure solve --solutions-in-one-file --number-of-solutions=all --output-format=jsonstream --unnamed-symmetry-breaking=Quick-AllPermutations-Independently *.essence *.param -o conjure-output-function-unnamed-Quick-AllPermutations-Independently --copy-solutions=no
cat conjure-output-function-unnamed-Quick-AllPermutations-Independently/*.solutions.json
rm -rf conjure-output*

# conjure solve --solutions-in-one-file --number-of-solutions=all --output-format=jsonstream --unnamed-symmetry-breaking=Complete-AllPermutations-Independently *.essence *.param -o conjure-output-function-unnamed-Complete-AllPermutations-Independently
