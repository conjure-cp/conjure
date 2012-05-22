.PHONY: install prof-install buildtests runtests clean

install:
	time cabal configure
	time cabal build
	time cabal install

buildtests:
	# ghc-pkg unregister conjure-cp
	time cabal configure --enable-tests
	time cabal build

runtests:
	time cabal test

prof-install:
	time cabal configure --enable-library-profiling --enable-executable-profiling
	time cabal build
	time cabal install

clean:
	@cabal clean
	@ghc-pkg unregister conjure-cp
	@find . -name "*.hi" -delete
	@find . -name "*.o" -delete
