.PHONY: install rules clean

install:
	bash scripts/build/install.sh
	conjureBF makeRulesDB `find files/rules -type f`

rules:
	conjureBF makeRulesDB `find files/rules -type f`

clean:
	scripts/build/clean

