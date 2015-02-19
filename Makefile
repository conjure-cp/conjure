.PHONY: install preinstall refreeze ghci clean

install:
	@bash etc/build/install.sh

preinstall:
	@bash etc/build/version.sh
	@runhaskell etc/build/gen_Operator.hs

refreeze:
	( rm -rf cabal.sandbox.config cabal.config dist .cabal-sandbox && BUILD_TESTS=yes RUN_TESTS=yes make && cabal freeze )

ghci:
	@cabal exec ghci -- -isrc -isrc/test           \
	    -XNoImplicitPrelude                        \
	    -XOverloadedStrings                        \
	    -XScopedTypeVariables                      \
	    -XQuasiQuotes                              \
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
	    `find src -name *.hs | grep -v Main`

clean:
	@bash etc/build/clean.sh
