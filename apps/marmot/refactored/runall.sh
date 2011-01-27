#!/bin/bash

unset HANDOPT_BUILDSPLIT
unset HANDOPT_MAPSPLIT
unset HANDOPT_FULLPAR
unset DEEPCOPY
unset DODUMMYAML
unset LD_PRELOAD

# use hoard
export LD_PRELOAD="/home/stoledo/libhoard.so:/lib64/libdl-2.6.so"

export HANDOPT_BUILDSPLIT=true
#export HANDOPT_MAPSPLIT=true
#export HANDOPT_FULLPAR=true
export DEEPCOPY=true

rm -rf ./temp
mkdir temp

#echo "#h numthreads realtime cputime" > RESULTS.txt 

export GRIDSIZE=360

for threads in `seq 15 15`; do
  export NUMTHREADS=$threads
#  export NUMTHREADS=16

  wsc.new test_marmot2.ws -O3 -t -dot
  TPLUSONE=`expr $threads + 1`
  for index in `seq 1 5` ; do 
    echo running with -j $TPLUSONE
    ./query.exe -j $TPLUSONE -n 600 > temp/LOG_$threads
    export TMP=`$REGIMENTD/bin/extract_startend_real.sh temp/LOG_$threads`
    export TMP2=`$REGIMENTD/bin/extract_startend_times.sh temp/LOG_$threads`
    echo "$threads  $TMP  $TMP2  $index" >> RESULTS.txt
  done

done
