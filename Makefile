.PHONY: install rules clean

install:
	bash scripts/build/install.sh
	conjureBF makeRulesDB `find files/rules -type f`

rules:
	conjureBF makeRulesDB `find files/rules -type f`

refreeze:
	( rm -rf cabal.sandbox.config cabal.config dist .cabal-sandbox && BUILD_TESTS=yes RUN_TESTS=yes make && cabal freeze )

clean:
	scripts/build/clean

