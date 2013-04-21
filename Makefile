.PHONY: install clean

install:
	cabal update
	scripts/build/make -O

rules:
	conjureBF makeRulesDB `find files/rules -type f`

clean:
	scripts/build/clean

