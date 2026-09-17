rm -rf conjure-output* *.solutions *.solutions.json *.stats.json
conjure solve --solutions-in-one-file --output-format=jsonstream --unnamed-symmetry-breaking=Eager-Consecutive-Independently *.essence *.param --number-of-solutions=20
cat *.solutions.json
rm -rf conjure-output* *.solutions *.solutions.json *.stats.json
