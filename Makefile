.PHONY: install rules clean

CORES = 0

install:
	bash scripts/build/install.sh $(CORES)

rules:
	conjureBF makeRulesDB `find files/rules -type f`

clean:
	scripts/build/clean

