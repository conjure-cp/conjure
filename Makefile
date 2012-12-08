.PHONY: install clean

install:
	cabal update
	scripts/build/make -O
	conjure makeRulesDB `find files/rules -type f | grep -e ".rule$" -e ".repr$"`

clean:
	scripts/build/clean

