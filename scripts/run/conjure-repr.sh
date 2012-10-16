#!/bin/sh

conjure-repr `find files/rules -type f | grep -e ".rule$" -e ".repr$"` $@

