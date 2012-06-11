
cd ~/src/conjure-wd/bundles/conjure-toCore
cp ~/.cabal/bin/conjure-toCore .
upx conjure-toCore
zip -r ~/Dropbox/Public/conjure-toCore.zip conjure-toCore
rm -r conjure-toCore
