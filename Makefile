.PHONY: install rules clean

install:
	bash scripts/build/install.sh
	conjureBF makeRulesDB `find files/rules -type f`

rules:
	conjureBF makeRulesDB `find files/rules -type f`

refreeze:
	( export BUILD_TESTS=yes ; export RUN_TESTS=yes ; rm -rf cabal.sandbox.config cabal.config dist .cabal-sandbox && make && cabal freeze )

clean:
	scripts/build/clean

