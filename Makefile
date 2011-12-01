.PHONY: install runtests derivations clean install-prof install-hpc hpc-markup clean-hpc

ALL_HS := $(shell find src -name '*.hs')

install: derivations
	@cabal install

runtests:
	@dist/build/conjure-tests/conjure-tests testdata/*

derivations: src/Language/EssenceDerivations.hs

src/Language/EssenceDerivations.hs: src/Language/Essence.hs
	@echo "Deriving instances for src/Language/EssenceDerivations.hs"
	@cd src/Language; derive Essence.hs

clean:
	@rm src/Language/EssenceDerivations.hs
	@rm -rf dist

install-prof: derivations
	@cabal install --enable-executable-profiling

install-hpc: derivations
	@cabal install --ghc-options=-fhpc

hpc-markup: conjure-tests.tix
	hpc markup conjure-tests

conjure-tests.tix: install-hpc runtests

clean-hpc:
	@rm -r *.tix *.hs.html hpc_index* .hpc
