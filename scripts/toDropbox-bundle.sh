
rm ~/Dropbox/Public/conjure-macosx.zip

cp ~/.cabal/bin/conjure-all ~/src/conjure-wd/bundles/conjure-macosx
cp ~/.cabal/bin/conjure-repr ~/src/conjure-wd/bundles/conjure-macosx
cp ~/.cabal/bin/conjure-refn ~/src/conjure-wd/bundles/conjure-macosx

upx ~/src/conjure-wd/bundles/conjure-macosx/conjure-*

cp -r ~/src/conjure-wd/testsuite ~/src/conjure-wd/bundles/conjure-macosx
rm -r ~/src/conjure-wd/bundles/conjure-macosx/testsuite/ruleengine/rules-bogus
rm -r ~/src/conjure-wd/bundles/conjure-macosx/testsuite/ruleengine/specs
rm -r ~/src/conjure-wd/bundles/conjure-macosx/testsuite/valid
find ~/src/conjure-wd/bundles/conjure-macosx -name .DS_Store -delete

cd ~/src/conjure-wd/bundles
zip -r ~/Dropbox/Public/conjure-macosx.zip conjure-macosx

rm -rf ~/src/conjure-wd/bundles/conjure-macosx/conjure-all \
       ~/src/conjure-wd/bundles/conjure-macosx/conjure-repr \
       ~/src/conjure-wd/bundles/conjure-macosx/conjure-refn \
       ~/src/conjure-wd/bundles/conjure-macosx/testsuite
