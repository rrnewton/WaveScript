#!/bin/sh

if ! [ -f "$1" ]; then echo "File does not exist!"; exit -1; fi

FOO=`grep "run-to-tuplimit" $1 -n | sed 's/:/ /' | awk '{ print $1 }' `
BAR=`wc -l $1 | awk '{ print $1 }'`
SUB=$(( $BAR - $FOO + 1 ))
FINAL=`tail $1 -n $SUB | grep "elapsed real" | awk '{ print $1 }'`

echo $FINAL

