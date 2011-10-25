.PHONY: install derivations clean

install: derivations
	@cabal install

derivations: src/Language/EssenceDerivations.hs

src/Language/EssenceDerivations.hs: src/Language/Essence.hs
	@echo "Deriving instances for src/Language/EssenceDerivations.hs"
	@cd src/Language; derive Essence.hs

clean:
	@rm src/Language/EssenceDerivations.hs
	@rm -rf dist
