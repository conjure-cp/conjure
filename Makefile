
SHELL := /bin/bash

.PHONY: install-with-stack
install-with-stack:
	@bash etc/build/install-stack.sh
	@cp etc/hs-deps/stack-8.2.yaml stack.yaml
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

.PHONY: docs
docs:
	(cd docs; make conjure-help; make latexpdf; make singlehtml)

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
	    `find src -name '*.hs' | grep -v 'Main.hs' | grep -v '\.#'`

.PHONY: hlint
hlint:
	-@hlint -r `find src -name '*.hs' | grep -v LogFollow` \
	    -i "Use camelCase" \
	    -i "Reduce duplication" \
	    -i "Use &&" \
	    -i "Use ++" \
	    -i "Redundant return" \
	    -i "Monad law, left identity"
