
SHELL := /bin/bash

.PHONY: install-with-stack
install-with-stack:
	@bash etc/build/install-stack.sh
	@cp etc/hs-deps/stack-8.0.yaml stack.yaml
	@stack setup
	@bash etc/build/version.sh
	@stack runhaskell etc/build/gen_Operator.hs
	@stack runhaskell etc/build/gen_Expression.hs
	@stack install
	@rm stack.yaml

.PHONY: install-with-cabal
install-with-cabal:
	@bash etc/build/install.sh

.PHONY: preinstall
preinstall:
	@bash etc/build/version.sh
	@runhaskell etc/build/gen_Operator.hs
	@runhaskell etc/build/gen_Expression.hs

.PHONY: freeze
freeze:
	@bash etc/build/freeze-deps.sh

.PHONY: refreeze
refreeze:
	@make clean
	@BUILD_TESTS=yes make install-with-cabal
	@make freeze

.PHONY: clean
clean:
	@bash etc/build/clean.sh

.PHONY: ghci
ghci:
	@cabal exec ghci -- -isrc -isrc/test           \
	    -idist/build/autogen                       \
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

.PHONY: hlint
hlint:
	-@hlint -r `find src -name '*.hs' | grep -v LogFollow` \
	    -i "Use camelCase" \
	    -i "Reduce duplication" \
	    -i "Use &&" \
	    -i "Use ++" \
	    -i "Redundant return" \
	    -i "Monad law, left identity"

.PHONY: runtests_quick
runtests_quick:
	-dist/build/conjure-testing/conjure-testing --select-tests=quick -p Conjuring  +RTS -s > >(tee runtests_quick_Conjuring__stdout.log) 2> >(tee runtests_quick_Conjuring__stderr.log >&2)
	-tests/acceptAllOutputs.sh > /dev/null
	-dist/build/conjure-testing/conjure-testing --select-tests=quick -p Savile     +RTS -s > >(tee runtests_quick_SavileRow__stdout.log) 2> >(tee runtests_quick_SavileRow__stderr.log >&2)
	-tests/acceptAllOutputs.sh > /dev/null
	-dist/build/conjure-testing/conjure-testing --select-tests=quick -p Check      +RTS -s > >(tee runtests_quick_Checking___stdout.log) 2> >(tee runtests_quick_Checking___stderr.log >&2)
	-dist/build/conjure-testing/conjure-testing --select-tests=quick -p Validating +RTS -s > >(tee runtests_quick_Validating_stdout.log) 2> >(tee runtests_quick_Validating_stderr.log >&2)

.PHONY: runtests_slow
runtests_slow:
	-dist/build/conjure-testing/conjure-testing --select-tests=slow -p Conjuring  +RTS -s > >(tee runtests_slow_Conjuring__stdout.log) 2> >(tee runtests_slow_Conjuring__stderr.log >&2)
	-tests/acceptAllOutputs.sh > /dev/null
	-dist/build/conjure-testing/conjure-testing --select-tests=slow -p Savile     +RTS -s > >(tee runtests_slow_SavileRow__stdout.log) 2> >(tee runtests_slow_SavileRow__stderr.log >&2)
	-tests/acceptAllOutputs.sh > /dev/null
	-dist/build/conjure-testing/conjure-testing --select-tests=slow -p Check      +RTS -s > >(tee runtests_slow_Checking___stdout.log) 2> >(tee runtests_slow_Checking___stderr.log >&2)
	-dist/build/conjure-testing/conjure-testing --select-tests=slow -p Validating +RTS -s > >(tee runtests_slow_Validating_stdout.log) 2> >(tee runtests_slow_Validating_stderr.log >&2)

.PHONY: runtests_all
runtests_all:
	-dist/build/conjure-testing/conjure-testing --select-tests=all -p Conjuring  +RTS -s > >(tee runtests_all_Conjuring__stdout.log) 2> >(tee runtests_all_Conjuring__stderr.log >&2)
	-tests/acceptAllOutputs.sh > /dev/null
	-dist/build/conjure-testing/conjure-testing --select-tests=all -p Savile     +RTS -s > >(tee runtests_all_SavileRow__stdout.log) 2> >(tee runtests_all_SavileRow__stderr.log >&2)
	-tests/acceptAllOutputs.sh > /dev/null
	-dist/build/conjure-testing/conjure-testing --select-tests=all -p Check      +RTS -s > >(tee runtests_all_Checking___stdout.log) 2> >(tee runtests_all_Checking___stderr.log >&2)
	-dist/build/conjure-testing/conjure-testing --select-tests=all -p Validating +RTS -s > >(tee runtests_all_Validating_stdout.log) 2> >(tee runtests_all_Validating_stderr.log >&2)
