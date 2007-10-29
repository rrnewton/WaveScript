#!/bin/sh

## Used by benchmarking utilities.
## Pulls the user time out of log output from a run of "wsc"

if ! [ -f "$1" ]; then echo "File does not exist!"; exit -1; fi

UTIME=`cat $1  | grep "User time" | awk '{ print $3 }'`

# convert to milliseconds
echo "(* $UTIME 1000)" | petite -q


