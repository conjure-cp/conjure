import unittest, os, json, constants 
import ../src/util/util

suite "poop":
    test "minionStoreDump":
        echo "dassa"
        echo prettifyMinionStoreDump("[ [2,2],[4,5],[7,15] ]")