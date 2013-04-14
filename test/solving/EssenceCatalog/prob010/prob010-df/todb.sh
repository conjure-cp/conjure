ls *.stats | parallel --tag cat {} 2>&1 | runhaskell todb.hs prob010 | parallel -j1

