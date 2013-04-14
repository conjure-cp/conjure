#!/bin/bash

# run to generate *.param files from instances.txt
# seq -w 1 20 | bash mkInstances.sh

COMMAND=$( cat <<EOF
echo "language Essence 1.3" >   {1}.param ;
echo ""                     >>  {1}.param ;
echo "letting n be {1}"     >>  {1}.param ;
echo ""                     >>  {1}.param
EOF
)

parallel --colsep ' ' $COMMAND

