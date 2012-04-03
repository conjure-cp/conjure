.PHONY: install prof-install runtests clean

install:
	time cabal install

runtests:
	# time cabal install --only-dependencies
	time cabal configure --enable-tests
	time cabal build
	# @say "starting the test"
	time cabal test

prof-install:
	time cabal install --enable-library-profiling --enable-executable-profiling

clean:
	@cabal clean
	@find . -name "*.hi" -delete
	@find . -name "*.o" -delete
