#!/bin/bash

# [2008.10.05] This runs mfcc6 at each cutpoint and counts the dropped input/network events.
# It does this with PREFILTER disabled.

if ! [ -d logs ]; then mkdir logs; fi

export THEMOTE=/dev/ttyUSB0

for ((CUT=1; CUT<=6; CUT++)); do 
  export CUT  
  echo Running cut $CUT
  wstiny mfcc6_fixedpoint_fb.ws -split || exit 1
  
  echo "RUNNING pc-side listener... $CUT"
  (time ./query.exe /dev/ttyUSB0 telosb -n -1 &) &> logs/cut_$CUT
  #(time ./query.exe /dev/ttyUSB0 telosb -n 10 ) &> logs/cut_$CUT
  sleep 120
  killall query.exe
done 
