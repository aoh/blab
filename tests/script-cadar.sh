#!/bin/sh

# check that a given string occurs (which should be very likely)

echo '"c"[ad]{1,10}"r" 10' | $@ -n 1000 | grep cadar > /dev/null
