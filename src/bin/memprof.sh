#!/bin/bash

## This is a simple script for recording the memory usage of a process over time.

echo " ** memprof: Launching command $1"> /dev/stderr

########################################
# Relegate all its output to stdout, so that we can put ours to stderr.
$* &> /dev/stdout &
#$* &
########################################

PID=$!
echo " ** Pid of child $PID" > /dev/stderr

JOBNUM=1

#echo "# ProgSize MemSize SharedPages CodePages DataPages LibPages DirtyPages"
echo "# PeakMem CurrentMem"
while [ "`jobs -r`" != "" ];
do 
  #echo still running "`jobs -r`"
  #cat /proc/$PID/statm > /dev/stderr
  #cat /proc/$PID/status > /dev/stderr
  #cat /proc/$PID/stat > /dev/stderr
  #egrep "(VmHWM)|(VmRSS)" /proc/$PID/status > /dev/stderr
  stats=`egrep "Vm(HWM|RSS)" /proc/$PID/status`
  
  ## Don't know how efficient this is, but it's one way to tokenize without awk/etc:
  #echo full stats - $stats --  
  #eval set -- "$stats"
  #echo "stats $1 . $2 . $3 . $4 . $5 . $6 ."
  echo $stats | awk '{ print $2" "$5 }' > /dev/stderr
  sleep 1
done

wait $PID
#kill $PID
