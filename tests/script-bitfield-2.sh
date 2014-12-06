#!/bin/sh

$@ -e '0b00101010 10' | grep -q "*"

