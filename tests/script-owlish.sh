#!/bin/sh

# test upreferences by constructing small programs and checking that they 
# evaluate to a number

GRAM='

   output = "(let ((" @var " " [0-9] ")) (print (number? " num ")))"

   num = [0-9]{1,40}
       | "(" num-bop " " num " " num ")"
       | "(/ " num " " num-nonzero ")"
       | "(let ((" @var " " num ")) " num ")"

   # prefix with _ to avoid generating keywords
   var = "_"[a-z]{1,5}

   num-nonzero = "(let ((" \(@var\) " " num ")) (if (zero? " \1 ") 1 " \1"))"

   num-bop = "+" | "-" | "*"
'

PROG=$($@ -e "$GRAM")

echo $PROG > owlish-$$.scm

../owl-lisp/bin/ol -q owlish-$$.scm | grep -q '#true' && rm owlish-$$.scm

