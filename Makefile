.PHONY: install preinstall refreeze ghci clean runtests_quick runtests_slow runtests_all


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


runtests_quick:
	dist/build/conjure-testing/conjure-testing --select-tests quick --rerun-update                         +RTS -s > >(tee runtests_quick_stdout1.log) 2> >(tee runtests_quick_stderr1.log >&2)
	tests/acceptAllOutputs.sh > /dev/null
	dist/build/conjure-testing/conjure-testing --select-tests quick --rerun-filter failures,exceptions,new +RTS -s > >(tee runtests_quick_stdout2.log) 2> >(tee runtests_quick_stderr2.log >&2)
	tests/acceptAllOutputs.sh > /dev/null
	dist/build/conjure-testing/conjure-testing --select-tests quick --rerun-filter failures,exceptions,new +RTS -s > >(tee runtests_quick_stdout3.log) 2> >(tee runtests_quick_stderr3.log >&2)

runtests_slow:
	dist/build/conjure-testing/conjure-testing --select-tests slow  --rerun-update                         +RTS -s > >(tee runtests_slow_stdout1.log)  2> >(tee runtests_slow_stderr1.log  >&2)
	tests/acceptAllOutputs.sh > /dev/null
	dist/build/conjure-testing/conjure-testing --select-tests slow  --rerun-filter failures,exceptions,new +RTS -s > >(tee runtests_slow_stdout2.log)  2> >(tee runtests_slow_stderr2.log  >&2)
	tests/acceptAllOutputs.sh > /dev/null
	dist/build/conjure-testing/conjure-testing --select-tests slow  --rerun-filter failures,exceptions,new +RTS -s > >(tee runtests_slow_stdout3.log)  2> >(tee runtests_slow_stderr3.log  >&2)

runtests_all:
	dist/build/conjure-testing/conjure-testing --select-tests all   --rerun-update                         +RTS -s > >(tee runtests_all_stdout1.log)   2> >(tee runtests_all_stderr1.log   >&2)
	tests/acceptAllOutputs.sh > /dev/null
	dist/build/conjure-testing/conjure-testing --select-tests all   --rerun-filter failures,exceptions,new +RTS -s > >(tee runtests_all_stdout2.log)   2> >(tee runtests_all_stderr2.log   >&2)
	tests/acceptAllOutputs.sh > /dev/null
	dist/build/conjure-testing/conjure-testing --select-tests all   --rerun-filter failures,exceptions,new +RTS -s > >(tee runtests_all_stdout3.log)   2> >(tee runtests_all_stderr3.log   >&2)
