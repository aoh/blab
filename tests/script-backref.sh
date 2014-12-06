#!/bin/sh

# check that backreferences work within a repetitoin, 
# they don't overwrite the previous ones and the last one
# is visible after the pattern (so that a repetition of one 
# behaves as if it were written without the repetition).

$@ -e '\("foo"\)(\("a" | "b"\) \2){10} \2 \1' | grep -qE 'foo(aa|bb){9}(aaa|bbb)foo' || exit 1 

