#!/bin/bash

# [2008.10.05] This runs mfcc6 at each cutpoint and counts the dropped input/network events.
# It does this with PREFILTER disabled.

if ! [ -d logs ]; then mkdir logs; fi

#rm -f logs/cut*

#export THEMOTE=/dev/ttyUSB0
MINUTES=4

STRT=1
STOP=6

for ((CUT=$STRT; CUT<=$STOP; CUT++)); do 
#for CUT in 1 2 3  6; do 
  echo Running cut $CUT
  export CUT  
  rm -rf build/telos*

  unset THEMOTE

  # Prefilter off currently.
  (SILENTROOT=t DUMMY=t wstiny mfcc6_fixedpoint_fb.ws -split -tree &> logs/compile_$CUT) || exit 1

  # Program both motes.  
  # Retry a couple times.
  VICTORY=0
  for try in 1 2; do
    ./progtelos all && VICTORY=1 && break
    # Otherwise retry:
    echo "RETRYING"
    rm -rf build/telos*
  done
  if [ $VICTORY = "0" ];
  then echo "Could not program motes successfully!"; exit 1;
  fi

  echo "RUNNING pc-side listener... $CUT"
  (time ./query.exe /dev/ttyUSB0 telosb -n -1 &) &> logs/cut_$CUT
  #(time ./query.exe /dev/ttyUSB0 telosb -n 10 ) &> logs/cut_$CUT
  sleep $((MINUTES * 60))
  killall query.exe
done 
