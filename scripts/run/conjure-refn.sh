#!/bin/sh

conjure-refn `find files/rules -type f | grep -e ".rule$" -e ".repr$"` $@

