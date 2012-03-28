.PHONY: ghci install clean runtests

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

ghci: src/Language/Essence.o
	# @ghci -isrc src/Language/Essence.hs

clean:
	@cabal clean
	@find . -name "*.hi" -delete
	@find . -name "*.o" -delete
