# rm -r ~/Dropbox/Public/conjure-wd
# rm -r ~/Dropbox/Public/conjure-wd.zip
# cp -r ~/src/conjure-wd ~/Dropbox/Public/
# rm -r ~/Dropbox/Public/conjure-wd/dist
# rm -r ~/Dropbox/Public/conjure-wd/.hg
# cd ~/Dropbox/Public
# zip -r conjure-wd conjure-wd

cd
cd src
zip -r ~/Dropbox/Public/conjure-wd.zip conjure-wd/Makefile
zip -r ~/Dropbox/Public/conjure-wd.zip conjure-wd/conjure.cabal
zip -r ~/Dropbox/Public/conjure-wd.zip conjure-wd/datafiles
zip -r ~/Dropbox/Public/conjure-wd.zip conjure-wd/figlets
zip -r ~/Dropbox/Public/conjure-wd.zip conjure-wd/problem-specs/*/*.essence
zip -r ~/Dropbox/Public/conjure-wd.zip conjure-wd/problem-specs/*/*.param
zip -r ~/Dropbox/Public/conjure-wd.zip conjure-wd/rules/refn/*.rule
zip -r ~/Dropbox/Public/conjure-wd.zip conjure-wd/rules/repr/*.repr
zip -r ~/Dropbox/Public/conjure-wd.zip conjure-wd/scripts
zip -r ~/Dropbox/Public/conjure-wd.zip conjure-wd/src
zip -r ~/Dropbox/Public/conjure-wd.zip conjure-wd/src-exec
zip -r ~/Dropbox/Public/conjure-wd.zip conjure-wd/testdata/*.essence
