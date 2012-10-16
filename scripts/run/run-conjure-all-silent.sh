#!/bin/sh

filename=$1
filename_noext=${filename%\.*}

conjure-all-silent \
    `find files/rules -type f | grep -e ".rule$" -e ".repr$"` \
    $filename +RTS -s 2> $filename_noext.rts

