#!/bin/bash

runhaskell -Wall build_db.hs $(find experiments/dontcare/all-combinations/size1 -name *.minion-stdout | grep -v partition-partition) | tee db_size1.csv
# runhaskell -Wall build_db.hs $(find experiments/dontcare/all-combinations/size2 -name *.minion-stdout | grep -v partition-partition) | tee db_size2.csv
# runhaskell -Wall build_db.hs $(find experiments/dontcare/all-combinations/size3 -name *.minion-stdout | grep -v partition-partition) | tee db_size3.csv
# runhaskell -Wall build_db.hs $(find experiments/dontcare/all-combinations/size4 -name *.minion-stdout | grep -v partition-partition) | tee db_size4.csv
# runhaskell -Wall build_db.hs $(find experiments/dontcare/all-combinations/size5 -name *.minion-stdout | grep -v partition-partition) | tee db_size5.csv

runhaskell build_db.hs $(find experiments3/dontcare/dominating-queens/*/*.info) | tee db_dominating-queens.csv
