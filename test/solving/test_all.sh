#/bin/bash

set -o nounset
set -o errexit

rm -f fail.txt pass.txt all.txt
touch fail.txt pass.txt all.txt

ls -1 */*.essence | parallel -u " cd {//} ; bash ../test_single.sh df ; cd .. ; cat {//}/fail.txt | sed -e 's/^/{//} /' >> fail.txt ; cat {//}/pass.txt | sed -e 's/^/{//} /' >> pass.txt ; cat {//}/all.txt | sed -e 's/^/{//} /' >> all.txt "

cat fail.txt | wc -l > countFail.txt
cat pass.txt | wc -l > countPass.txt
cat all.txt  | wc -l > countAll.txt

