#/bin/sh

find . -name "*.prof" -delete
find . -name "*.hi" -delete
find . -name "*.o" -delete
find . -name "*.hi-boot" -delete
find . -name "*.o-boot" -delete
rm -f src/RepositoryVersion.hs
rm -rf dist ~/.cabal-sandbox cabal.sandbox.config
