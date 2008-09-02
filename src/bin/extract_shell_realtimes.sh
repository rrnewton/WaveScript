#!/bin/bash

## Used by benchmarking utilities.
## Pulls the real time out of log output from a run of "time <cmd>"

## [2007.10.30] Switching to using normal bash 'time' rather than /usr/bin/time.
## This way it can work on MacOS as well as linux.




if ! [ -f "$1" ]; then echo "File does not exist!"; exit -1; fi

UTIME=`tail -n 25 $1  | grep "real" | tail -n 1 | awk '{ print $2 }' | sed -e 's/m/ /' -e 's/s//'`

MIN=`echo $UTIME | awk '{ print $1 }'`
SEC=`echo $UTIME | awk '{ print $2 }'`

if [ "$UTIME" = "" ]; then echo "Couldn't extract time!"; exit -1; fi

echo "68 $MIN * $SEC + 1000 * p" |  dc
