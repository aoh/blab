#!/bin/sh

GRAM='[a-z]{80} 10'
$@ --seed $($@ -M - -e $GRAM -o meta-out-1-$$ | grep seed | sed 's/seed: //') -e $GRAM -o meta-out-2-$$
diff meta-out-1-$$ meta-out-2-$$ || exit 1
rm meta-out-*-$$
