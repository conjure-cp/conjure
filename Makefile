.PHONY: install prof-install buildtests runtests clean

install:
	# ghc-pkg unregister conjure-cp
	time cabal install --disable-library-profiling --disable-executable-profiling --force-reinstalls
	# upx ~/.cabal/bin/conjure-toCore

buildtests:
	# ghc-pkg unregister conjure-cp
	time cabal configure --enable-tests --disable-library-profiling --disable-executable-profiling
	time cabal build

runtests:
	time cabal test

prof-install:
	# ghc-pkg unregister conjure-cp
	time cabal configure --enable-library-profiling --enable-executable-profiling
	time cabal build
	time cabal install

prof-buildtests:
	# ghc-pkg unregister conjure-cp
	# time cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests --ghc-options="-with-rtsopts -xc" # gives a stack trace in case of runtime error.
	time cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests
	time cabal build

clean:
	cabal clean
	# ghc-pkg unregister conjure-cp
	find . -name "*.hi" -delete
	find . -name "*.o" -delete
