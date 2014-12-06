#!/bin/sh

# 1 rule with 2 options + 2 uprefs -> 2 options

$@ -e 'foo = @bar baz 10  bar = (48 | 49) baz=\bar \bar' -n 50 | sort | uniq | wc -l | grep -q 2 || exit 1

