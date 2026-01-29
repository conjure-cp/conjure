rm -rf conjure-output
conjure solve test.essence --solver=minion --solutions-in-one-file --number-of-solutions=10000 --copy-solutions=no
rm -rf conjure-output
