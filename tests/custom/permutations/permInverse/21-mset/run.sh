rm -rf conjure-output* *.solutions *.solutions.json
conjure solve --solutions-in-one-file --output-format=jsonstream --unnamed-symmetry-breaking=Complete-Consecutive-Independently *.essence *.param --number-of-solutions=20
cat *.solutions.json
rm -rf conjure-output* *.solutions *.solutions.json
