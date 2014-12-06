#!/bin/sh

radamsa -o :31337 tests/*.unit 2>&1 > /dev/null &

TIMEOUT=152 # return value when being killed by timeout (128 + SIGXCPU=24)
TIME=5

while true
do
   nc localhost 31337 | tee bad | (ulimit -S -t $TIME; blab)
   RVAL=$?
   # fatal signals are >127, but also unexpected vm-thrown errors would be interesting (126 & 127)
   test $RVAL -gt 120 && test $RVAL != $TIMEOUT && break "ERROR: input at ./bad causes rval $RVAL"
done

kill -9 %1

