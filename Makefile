.PHONY: install runtests clean
		install-prof runtests-prof
		install-hpc runtests-hpc clean-hpc
		derivations

ALL_HS := $(shell find src -name '*.hs')


install: derivations
	@cabal install

runtests:
	@dist/build/conjure-tests/conjure-tests testdata/* rules/repr/*

clean:
	@rm -rf src/Language/EssenceDerivations.hs dist


install-prof: derivations
	@cabal install --enable-executable-profiling --enable-library-profiling --ghc-option=-auto-all

runtests-prof:
	@dist/build/conjure-tests/conjure-tests testdata/* +RTS -L50 -hy
	@hp2ps -c conjure-tests.hp
	@ps2pdf conjure-tests.ps

clean-prof:
	@rm -f conjure-tests.prof conjure-tests.hp conjure-tests.aux conjure-tests.ps conjure-tests.pdf

install-hpc: derivations
	@cabal install --ghc-options=-fhpc

runtests-hpc:
	@dist/build/conjure-tests/conjure-tests testdata/*
	@hpc markup conjure-tests

clean-hpc:
	@rm -rf *.tix *.hs.html hpc_index* .hpc


clean-all: clean clean-prof clean-hpc

derivations: src/Language/EssenceDerivations.hs

src/Language/EssenceDerivations.hs: src/Language/Essence.hs
	@echo "Deriving instances for src/Language/EssenceDerivations.hs"
	@cd src/Language; derive Essence.hs
