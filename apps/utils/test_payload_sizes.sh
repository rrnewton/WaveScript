#!/bin/bash

# [2008.10.08] The largest working message size was 106 bytes just using the serial messages.
# Because I always have to go through serial (even if using CTP),  this will be an upper bound.

#LOG=./serial_msg_only.txt
LOG=./serial_msg_big_bytes.txt

MOTE=0

rm -f $LOG

# This tests how much overhead we need:

#for ((OVERHEAD=28; OVERHEAD >= 2; OVERHEAD -= 2)); do
#for ((OVERHEAD=2; OVERHEAD <= 28; OVERHEAD += 2)); do
for ((BYTES=12; BYTES <= 200; BYTES += 2)); do
  (echo;echo;echo;) >> $LOG
  export OVERHEAD=10
  #export OVERHEAD
  export BYTES
  #export BYTES=28

  echo "RUNNING FOR OVEARHEAD = $OVERHEAD  BYTES = $BYTES"
  echo "RUNNING FOR OVEARHEAD = $OVERHEAD  BYTES = $BYTES" >> $LOG

  wstiny test_payload_size.ws -split || (echo "Compile failed >> $LOG")
  echo Compiled...

  ./progtelos $MOTE || (echo "Progtelos failed >> $LOG")
  # >> $LOG 2>> $LOG
  echo Programmed...

  (./query.exe /dev/ttyUSB"$MOTE" telosb -n 10 >> $LOG 2>> $LOG) &
  sleep 20
  echo "Waking up to kill process if it is still there"
  ps aux | grep query.exe
  kill %1
done
