
echo "repr $1"
./run-repr.sh $1
ls $1-repr/*.essence   | parallel "echo 'refn {}' ; ./run-refn.sh {}"
ls $1-repr/*/*.essence | parallel "./run-all2.sh {}"
