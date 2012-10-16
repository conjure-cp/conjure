.PHONY: install clean

install:
	time scripts/build/make -j4

clean:
	scripts/build/clean

