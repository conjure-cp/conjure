.PHONY: install preinstall refreeze ghci clean runtests_once_quick runtests_once_slow runtests_once_all runtests_quick runtests_slow runtests_all


install:
	@bash etc/build/install.sh

preinstall:
	@bash etc/build/version.sh
	@runhaskell etc/build/gen_Operator.hs
	@runhaskell etc/build/gen_Expression.hs

refreeze:
	( rm -rf cabal.sandbox.config cabal.config dist .cabal-sandbox && BUILD_TESTS=yes RUN_TESTS=yes make && cabal freeze )

ghci:
	@cabal exec ghci -- -isrc -isrc/test           \
	    -XFlexibleContexts                         \
	    -XFlexibleInstances                        \
	    -XMultiParamTypeClasses                    \
	    -XNoImplicitPrelude                        \
	    -XOverloadedStrings                        \
	    -XQuasiQuotes                              \
	    -XScopedTypeVariables                      \
	    -XTypeOperators                            \
	    -XLambdaCase                               \
	    -XMultiWayIf                               \
	    -fwarn-incomplete-patterns                 \
	    -fwarn-incomplete-uni-patterns             \
	    -fwarn-missing-signatures                  \
	    -fwarn-name-shadowing                      \
	    -fwarn-orphans                             \
	    -fwarn-overlapping-patterns                \
	    -fwarn-tabs                                \
	    -fwarn-unused-do-bind                      \
	    -fwarn-unused-matches                      \
	    -Wall                                      \
	    -Werror                                    \
	    `find src -name *.hs | grep -v Main.hs | grep -v '\.#'`

clean:
	@bash etc/build/clean.sh


runtests_once_quick:
	dist/build/conjure-testing/conjure-testing --select-tests quick --rerun-update +RTS -s

runtests_once_slow:
	dist/build/conjure-testing/conjure-testing --select-tests slow --rerun-update +RTS -s

runtests_once_all:
	dist/build/conjure-testing/conjure-testing --select-tests all --rerun-update +RTS -s


runtests_quick:
	dist/build/conjure-testing/conjure-testing --select-tests quick --rerun-update +RTS -s
	tests/acceptAllOutputs.sh > /dev/null
	dist/build/conjure-testing/conjure-testing --select-tests quick --rerun-filter failures,exceptions,new +RTS -s
	tests/acceptAllOutputs.sh > /dev/null
	dist/build/conjure-testing/conjure-testing --select-tests quick --rerun-filter failures,exceptions,new +RTS -s

runtests_slow:
	dist/build/conjure-testing/conjure-testing --select-tests slow --rerun-update +RTS -s
	tests/acceptAllOutputs.sh > /dev/null
	dist/build/conjure-testing/conjure-testing --select-tests slow --rerun-filter failures,exceptions,new +RTS -s
	tests/acceptAllOutputs.sh > /dev/null
	dist/build/conjure-testing/conjure-testing --select-tests slow --rerun-filter failures,exceptions,new +RTS -s

runtests_all:
	dist/build/conjure-testing/conjure-testing --select-tests all --rerun-update +RTS -s
	tests/acceptAllOutputs.sh > /dev/null
	dist/build/conjure-testing/conjure-testing --select-tests all --rerun-filter failures,exceptions,new +RTS -s
	tests/acceptAllOutputs.sh > /dev/null
	dist/build/conjure-testing/conjure-testing --select-tests all --rerun-filter failures,exceptions,new +RTS -s
