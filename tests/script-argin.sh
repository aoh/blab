#!/bin/sh

fail() {
   echo "FAILURE: $@"
   exit 1
}

# check that command line input behaves like stdin input

SEED=$$

A=`echo "foo = bar* baz+ 10 bar = 42 | 97 baz = 43" | $@ -s $SEED` 
B=`$@ -s $SEED -e "foo = bar* baz+ 10 bar = 42 | 97 baz = 43"`
C=`$@ -s $SEED -e "foo = bar* baz+ 10" "bar = 42 | 97" "baz = 43"`

test "$A" = "$B" && test "$B" = "$C" || fail "outputs differ: '$A' vs '$B' vs '$C'"

