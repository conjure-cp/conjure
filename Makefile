.PHONY: install refreeze clean

install:
	bash scripts/build/install.sh

refreeze:
	( rm -rf cabal.sandbox.config cabal.config dist .cabal-sandbox && BUILD_TESTS=yes RUN_TESTS=yes make && cabal freeze )

clean:
	scripts/build/clean

