
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
BASENAME=`basename $DIR`

cd $DIR
rm conjure-toCore
cd ..
curl http://dl.dropbox.com/u/14272760/conjure-toCore.zip -o conjure-toCore.zip
unzip -o conjure-toCore.zip
rm conjure-toCore.zip
cd conjure-toCore
