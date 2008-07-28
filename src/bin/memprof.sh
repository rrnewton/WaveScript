#!/bin/bash

## This is a simple script for recording the memory usage of a process over time.
## LINUX ONLY!

#echo " ** memprof: Launching command $1"> /dev/stderr

########################################
# Relegate all its output to stdout, so that we can put ours to stderr.
$* &> /dev/stdout &
#$* &
########################################

PID=$!
#echo " ** Pid of child $PID" > /dev/stderr

JOBNUM=1

#echo "# ProgSize MemSize SharedPages CodePages DataPages LibPages DirtyPages"


echo "# Selected fields from /proc/<pid>/status: " > /dev/stderr

header=`egrep "Vm(HWM|RSS|Data|Stk|Exe|Lib)" /proc/$$/status | awk '{ print $1 }' | xargs echo `
echo "# $header" > /dev/stderr

#echo "# PeakMem ResidentMem DataMem" > /dev/stderr
while [ "`jobs -r`" != "" ];
do 

  #cat /proc/$PID/statm > /dev/stderr  

  #stats=`egrep "Vm(HWM|RSS|Data)" /proc/$PID/status`
  #stats=`egrep "Vm(HWM|RSS|Data|Stk|Exe|Lib)" /proc/$PID/status | awk '{ print $2 }' | xargs echo `
  egrep "Vm(HWM|RSS|Data|Stk|Exe|Lib)" /proc/$PID/status | awk '{ print $2 }' | xargs echo > /dev/stderr

  ##echo $stats | awk '{ print $2" "$5" "$8 }' > /dev/stderr

  #stats=`egrep "Vm" /proc/$PID/status`
  #echo $stats > /dev/stderr  

  sleep 1
done

wait $PID
#kill $PID
