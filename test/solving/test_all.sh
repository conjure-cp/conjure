#/bin/bash

ls -1 */*.essence | parallel -j1 " cd {//} ; bash ../test_single.sh df "

