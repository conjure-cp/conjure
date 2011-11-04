.PHONY: install runtests derivations clean

ALL_HS := $(shell find src -name '*.hs')

install: derivations
	@cabal install

runtests: dist/build/conjure-tests/conjure-tests
	@dist/build/conjure-tests/conjure-tests testdata/*

derivations: src/Language/EssenceDerivations.hs

dist/build/conjure-tests/conjure-tests: $(ALL_HS)
	@make install

dist/build/conjure-tests/iconjure: $(ALL_HS)
	@make install

src/Language/EssenceDerivations.hs: src/Language/Essence.hs
	@echo "Deriving instances for src/Language/EssenceDerivations.hs"
	@cd src/Language; derive Essence.hs

clean:
	@rm src/Language/EssenceDerivations.hs
	@rm -rf dist
