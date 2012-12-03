.PHONY: install clean

install:
	cabal update
	scripts/build/make -O

clean:
	scripts/build/clean

