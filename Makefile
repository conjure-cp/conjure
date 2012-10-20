.PHONY: install clean

install:
	time scripts/build/make -j4 -O

clean:
	scripts/build/clean

