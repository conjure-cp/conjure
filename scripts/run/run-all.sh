#!/bin/sh

conjure-all `find testsuite/ruleengine/rules -name "*.repr"` \
            `find testsuite/ruleengine/rules -name "*.rule"` $@
