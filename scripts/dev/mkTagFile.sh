#/bin/sh

# runs mkTagFile.hs to generate src/Stuff/Generic/Tag.hs

runhaskell scripts/dev/mkTagFile.hs < src/Stuff/Generic/ConstructorTags.txt > src/Stuff/Generic/Tag.hs

