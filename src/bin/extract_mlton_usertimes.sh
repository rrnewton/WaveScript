#!/bin/sh

## Used by benchmarking utilities.
## Pulls the user time out of log output from a run of "wsmlton"

## NOTE: mlton doesn't self-time yet... we just assume that the query was run with:
#  /usr/bin/time -f "usertime %S\nrealtime %e\n"

if ! [ -f "$1" ]; then echo "File does not exist!"; exit -1; fi

UTIME=`tail -n 10 $1  | grep "usertime" | awk '{ print $2 }'`

if [ "$UTIME" == "" ]; then echo "Couldn't extract time!"; exit -1; fi

# convert to milliseconds
echo "(* (+ 0 $UTIME) 1000)" | petite -q
