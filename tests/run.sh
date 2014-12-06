#!/bin/sh

fail(){
   echo $1
   exit 1
}

BLAB=$@

for test in *.unit
do
   echo -n " + $test"
   GOT=`cat $test | $BLAB || echo "(but blab exited with $?)"`
   WANT=`cat $test.expected`
   if [ "$GOT" = "$WANT" ]
   then
      echo " ok"
   else 
      fail "test $test fails: got '$GOT' instead of '$WANT'"
   fi
done 

for bad in *.bad
do
   echo -n " - $bad:"
   $BLAB < $bad > /dev/null 2>&1 && fail " !! test fails: $bad" || echo " ok"
done

for script in script-*.sh
do
   echo -n " o $script: "
   sh $script $@ && echo "ok" || fail "script test fails: $script"
done
