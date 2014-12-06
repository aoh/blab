#!/bin/sh

echo '34 "a"{100000} 34'| $@ | $@ > large-$$-1
echo '"a"{100000}'| $@ > large-$$-2

cmp large-$$-1 large-$$-2 || exit 1

rm large-$$-1 large-$$-2
