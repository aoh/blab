#!/bin/sh

A=`echo 'S = [a-z]' | $@ -n 256 -s 3142`
B=`echo 'S = [a-z]' | $@ -n 256 -s 3142`

test "$A" = "$B"
