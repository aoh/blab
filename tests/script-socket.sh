#!/bin/sh

for foo in $(seq 10)
do
   PORT=$(../owl-lisp/bin/ol -e '(lets ((rs a (rand-range (seed->rands (time-ms)) 49152 65536))) a)')
   $@ -e 'o = a b c a = "slarti" b = "bart" c = "fast"' -o :$PORT &
   SUB=$!
   sleep 0.2
   nc localhost $PORT < /dev/null | grep -q slartibartfast && exit 0
   kill -9 $SUB
   sleep 0.2
done

echo badness

exit 1
