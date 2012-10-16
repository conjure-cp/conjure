#!/bin/sh

conjure-all `find files/rules -type f | grep -e ".rule$" -e ".repr$"` $@

