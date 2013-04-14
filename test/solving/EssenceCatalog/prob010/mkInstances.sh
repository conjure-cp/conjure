#!/bin/bash

# run to generate *.param files from instances.txt
# cat instances.txt | bash mkInstances.sh

COMMAND=$( cat <<EOF
echo "language Essence 1.3" >   {1}-{2}-{3}.param ;
echo ""                     >>  {1}-{2}-{3}.param ;
echo "letting w be {1}"     >>  {1}-{2}-{3}.param ;
echo "letting g be {2}"     >>  {1}-{2}-{3}.param ;
echo "letting s be {3}"     >>  {1}-{2}-{3}.param ;
echo ""                     >>  {1}-{2}-{3}.param
EOF
)

parallel --colsep ' ' $COMMAND

