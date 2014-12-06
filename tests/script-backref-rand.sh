#!/bin/sh

# seems to hit a combinatorial bomb in grep if any more chars are added

$@ -e 'o = "<" \(w\)\(w\)\(w\)(\?){10} ">" 10 w = [a-z]{1,2}' | grep -q -E '^<([a-z]{1,2})([a-z]{1,2})([a-z]{1,2})(\1|\2|\3){10}>$'

