#!/bin/sh

# 2 rules with 2 options + 2 uprefs -> 10 options

$@ -e 'foo = @bar @bar baz 10  bar = (48 | 49) baz=\bar \bar' -n 500 | sort | uniq | wc -l | grep -q 10 || exit 1

