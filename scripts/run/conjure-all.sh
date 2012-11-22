#!/bin/sh

filename=$1
filename_noext=${filename%\.*}

mkdir -p $filename_noext

conjure-all \
    `find files/rules -type f | grep -e ".rule$" -e ".repr$"` \
        $filename +RTS -s -K100M -M2G

