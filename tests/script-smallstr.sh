#!/bin/sh

echo '34 "a"{4096} 34'| $@ | $@ > small-$$-1
echo '"a"{4096}'| $@ > small-$$-2

cmp small-$$-1 small-$$-2 || exit 1

rm small-$$-*
