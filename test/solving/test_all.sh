#/bin/bash

set -o nounset
set -o errexit

ls -1 */*.essence | parallel -j1 " cd {//} ; bash ../test_single.sh df "

