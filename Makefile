
SHELL := /bin/bash

# these are default values
# override by calling the makefile like so: "GHC_VERSION=8.2 make"
export GHC_VERSION?=8.2
export BIN_DIR?=${HOME}/.local/bin
export CI?=false
export BUILD_TESTS?=false

.PHONY: install
install:
	@echo Using Stack file: etc/hs-deps/stack-${GHC_VERSION}.yaml
	@if ${BUILD_TESTS} ; then echo "BUILD_TESTS=true"; fi
	@if ${CI} ; then echo "CI=true"; fi
	@bash etc/build/install-stack.sh
	@cp etc/hs-deps/stack-${GHC_VERSION}.yaml stack.yaml
	@if  [ ${GHC_VERSION} == "head" ] ; then\
		stack setup --resolver nightly;\
	else\
		stack setup;\
	fi
	@bash etc/build/version.sh
	@stack runhaskell etc/build/gen_Operator.hs
	@stack runhaskell etc/build/gen_Expression.hs
	@if  [ ${GHC_VERSION} == "head" ] &&   ${BUILD_TESTS} &&   ${CI} ; then\
		stack install --resolver nightly --test --no-run-tests --no-terminal;\
	elif [ ${GHC_VERSION} == "head" ] &&   ${BUILD_TESTS} && ! ${CI} ; then\
		stack install --resolver nightly --test --no-run-tests;\
	elif [ ${GHC_VERSION} == "head" ] && ! ${BUILD_TESTS} &&   ${CI} ; then\
		stack install --resolver nightly --no-terminal;\
	elif [ ${GHC_VERSION} == "head" ] && ! ${BUILD_TESTS} && ! ${CI} ; then\
		stack install --resolver nightly;\
	elif   ${BUILD_TESTS} &&   ${CI} ; then\
		stack install --test --no-run-tests --no-terminal;\
	elif   ${BUILD_TESTS} && ! ${CI} ; then\
		stack install --test --no-run-tests;\
	elif ! ${BUILD_TESTS} &&   ${CI} ; then\
		stack install --no-terminal;\
	elif ! ${BUILD_TESTS} && ! ${CI} ; then\
		stack install;\
	fi
	@rm stack.yaml

.PHONY: install-using-cabal
install-using-cabal:
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
	@BUILD_TESTS=yes make install-using-cabal
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
