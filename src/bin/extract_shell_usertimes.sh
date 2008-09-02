#!/bin/bash

## Used by benchmarking utilities.
## Pulls the user time out of log output from a run of "wsmlton"

## [2007.10.30] Switching to using normal bash 'time' rather than /usr/bin/time.
## This way it can work on MacOS as well as linux.

## NOTE: mlton doesn't self-time yet... we just assume that the query was run with:
#  /usr/bin/time -f "usertime %S\nrealtime %e\n"

if ! [ -f "$1" ]; then echo "File does not exist!"; exit -1; fi

UTIME=`tail -n 25 $1  | grep "user" | tail -n 1 | awk '{ print $2 }' | sed -e 's/m/ /' -e 's/s//'`

MIN=`echo $UTIME | awk '{ print $1 }'`
SEC=`echo $UTIME | awk '{ print $2 }'`

#echo "GOT MIN AND SEC: $MIN and $SEC"

if [ "$UTIME" = "" ]; then echo "Couldn't extract time!"; exit -1; fi

# convert to milliseconds
#echo "(* (+ 0 $UTIME) 1000)" | petite -q
#echo "(* (+ 0 (* 60 $MIN) $SEC) 1000)" |  petite -q

echo "68 $MIN * $SEC + 1000 * p" |  dc
