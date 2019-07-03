import unittest, os, json, constants 
import ../src/util/util

suite "poop":
    test "minionStoreDump":
        check(prettifyMinionStoreDump("[ [2,2],[4,5],[7,15] ]") == "int(2, 4..5, 7..15)")