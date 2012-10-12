
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
BASENAME=`basename $DIR`

cd $DIR
rm -rf conjure-macosx
cd ..
curl http://dl.dropbox.com/u/14272760/conjure-macosx.zip -o conjure-macosx.zip
unzip -o conjure-macosx.zip
rm conjure-macosx.zip
cd conjure-macosx
cd `pwd`
