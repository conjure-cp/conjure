.PHONY: install prof-install buildtests runtests clean

quick-install:
	time cabal install --disable-library-profiling --disable-executable-profiling --disable-documentation --force-reinstalls -O0 --ghc-options="+RTS -M4G"

install:
	time cabal install --disable-library-profiling --disable-executable-profiling --disable-documentation --force-reinstalls -O2 --ghc-options="+RTS -M4G"

quick-buildtests:
	time cabal configure --enable-tests --disable-library-profiling --disable-executable-profiling -O0 --ghc-options="+RTS -M4G"
	time cabal build

buildtests:
	time cabal configure --enable-tests --disable-library-profiling --disable-executable-profiling -O2 --ghc-options="+RTS -M4G"
	time cabal build

runtests:
	time dist/build/conjure-tests/conjure-tests

quick-prof-install:
	time cabal install --enable-library-profiling --enable-executable-profiling --disable-documentation --force-reinstalls -O0 --ghc-options="+RTS -M4G"

prof-install:
	time cabal install --enable-library-profiling --enable-executable-profiling --disable-documentation --force-reinstalls -O2 --ghc-options="+RTS -M4G"

prof-buildtests:
	time cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests -O2 --ghc-options="+RTS -M4G"
	time cabal build

prof-runtests:
	dist/build/conjure-tests/conjure-tests +RTS -hb && mv conjure-tests.hp debugging/conjure-tests-hb.hp && cd debugging && hp2ps -c conjure-tests-hb.hp && ps2pdf conjure-tests-hb.ps && rm conjure-tests-hb.aux
	dist/build/conjure-tests/conjure-tests +RTS -hc && mv conjure-tests.hp debugging/conjure-tests-hc.hp && cd debugging && hp2ps -c conjure-tests-hc.hp && ps2pdf conjure-tests-hc.ps && rm conjure-tests-hc.aux
	dist/build/conjure-tests/conjure-tests +RTS -hd && mv conjure-tests.hp debugging/conjure-tests-hd.hp && cd debugging && hp2ps -c conjure-tests-hd.hp && ps2pdf conjure-tests-hd.ps && rm conjure-tests-hd.aux
	dist/build/conjure-tests/conjure-tests +RTS -hm && mv conjure-tests.hp debugging/conjure-tests-hm.hp && cd debugging && hp2ps -c conjure-tests-hm.hp && ps2pdf conjure-tests-hm.ps && rm conjure-tests-hm.aux
	dist/build/conjure-tests/conjure-tests +RTS -hr && mv conjure-tests.hp debugging/conjure-tests-hr.hp && cd debugging && hp2ps -c conjure-tests-hr.hp && ps2pdf conjure-tests-hr.ps && rm conjure-tests-hr.aux
	dist/build/conjure-tests/conjure-tests +RTS -hy && mv conjure-tests.hp debugging/conjure-tests-hy.hp && cd debugging && hp2ps -c conjure-tests-hy.hp && ps2pdf conjure-tests-hy.ps && rm conjure-tests-hy.aux

deps:
	cabal install --only-dependencies --enable-library-profiling --disable-executable-profiling -O2 --enable-documentation

clean:
	-find . -name "*.prof" -delete
	-find . -name "*.hi" -delete
	-find . -name "*.o" -delete
	-find . -name "*.hi-boot" -delete
	-find . -name "*.o-boot" -delete
	-cabal clean
	-ghc-pkg unregister conjure-cp

mate:
	mate *.cabal *.sh Makefile src src-exec test testsuite EssenceCatalog bundles scripts
