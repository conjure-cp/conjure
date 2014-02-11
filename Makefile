.PHONY: install rules clean

install:
	bash scripts/build/install.sh

rules:
	conjureBF makeRulesDB `find files/rules -type f`

clean:
	scripts/build/clean

