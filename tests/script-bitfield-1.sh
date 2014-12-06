#!/bin/sh

$@ -e '0b01______ 10' -n 1000 | sort | uniq | wc -l | grep -q 64
