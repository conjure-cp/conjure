
spec=$1
timestamp=$(date "+%Y%m%d-%H%M")
mode=$2
title="$spec-$mode-$timestamp"
titleForFilename=$(echo "$title" | sed 's/\//-/g' | sed 's/\./-/g')
outputFilename="debugging/$titleForFilename.hp"

mkdir -p "debugging"

echo "will create: $outputFilename"
conjure-all "$spec" `find files/rules -type f | grep -e ".rule$" -e ".repr$"` +RTS "-$mode" -L100
echo "JOB \"$title\""      >  "$outputFilename"
tail -n +2 conjure-all.hp  >> "$outputFilename"
rm conjure-all.hp
echo "created: $outputFilename"

