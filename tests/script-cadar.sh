#!/bin/sh

# check that a given string occurs (which should be very likely)

echo '"c"[ad]*"r" 10' | $@ -n 1000 | grep cadar > /dev/null
