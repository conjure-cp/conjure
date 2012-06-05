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

prof-buildtests:
	# time cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests --ghc-options="-with-rtsopts -xc" # gives a stack trace in case of runtime error.
	time cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests
	time cabal build

clean:
	@cabal clean
	@ghc-pkg unregister conjure-cp
	@find . -name "*.hi" -delete
	@find . -name "*.o" -delete
