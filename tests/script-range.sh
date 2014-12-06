#!/bin/sh

# check that all chars from the range occur (which happens very likely)

echo "([abcd-f0-9x] 10){100}" | $@ -n 100 | sort | uniq | wc -l | grep -q 17

