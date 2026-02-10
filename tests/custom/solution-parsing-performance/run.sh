# this command went from taking ~50 seconds to taking ~5 seconds after introducing Conjure.Language.ParserCPrime
rm -rf conjure-output *.solution *.stats.json
conjure solve test.essence --solver=minion --solutions-in-one-file --number-of-solutions=5000 --copy-solutions=no
rm -rf conjure-output *.solution *.stats.json
