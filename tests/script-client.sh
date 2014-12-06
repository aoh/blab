#!/bin/sh

PORTARG=$(uname | grep -q BSD && echo "" || echo "-p")

for foo in $(seq 10)
do
   PORT=$(../owl-lisp/bin/ol -e '(lets ((rs a (rand-range (seed->rands (time-ms)) 49152 65536))) a)')
   nc -l $PORTARG $PORT > client-$$ &
   SUB=$!
   sleep 0.2
   $@ -e 'o = a b c a = "slarti" b = "bart" c = "fast"' -o 127.0.0.1:$PORT
   grep -q slartibartfast client-$$ && rm client-$$ && exit 0
   kill -9 $SUB
   sleep 0.2
done

rm client-$$
echo badness
exit 1
