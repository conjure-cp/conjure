#!/bin/sh

echo "repr $1"
./run-repr.sh $1
ls $1-repr/*.essence   2> /dev/null | parallel "echo 'refn {}' ; ./run-refn.sh {}"
ls $1-repr/*/*.essence 2> /dev/null | parallel "./run-all2.sh {}"
