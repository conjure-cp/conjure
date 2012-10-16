
rm ~/Dropbox/Public/conjure-macosx.zip

cp -r ~/src/conjure-wd/files ~/src/conjure-wd/bundles/conjure-macosx
rm -r ~/src/conjure-wd/bundles/conjure-macosx/files/conjure.figlet \
      ~/src/conjure-wd/bundles/conjure-macosx/files/testdata \
      ~/src/conjure-wd/bundles/conjure-macosx/files/EssenceCatalog

find ~/src/conjure-wd/bundles/conjure-macosx -name .DS_Store -delete
find ~/src/conjure-wd/bundles/conjure-macosx -name "*~" -delete
find ~/src/conjure-wd/bundles/conjure-macosx -name "*.swp" -delete

cd ~/src/conjure-wd/bundles
zip -r ~/Dropbox/Public/conjure-macosx.zip conjure-macosx

