.PHONY: install clean

install:
	cabal update
	scripts/build/make -O
	conjure makeRulesDB \
		`find files/rules -name "*.rule"` \
		`find files/rules -name "*.repr"`

clean:
	scripts/build/clean

