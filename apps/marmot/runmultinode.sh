#!/bin/bash

unset HANDOPT_BUILDSPLIT
unset HANDOPT_MAPSPLIT
unset HANDOPT_FFTIFFT

source refactored/shared.sh

rm -rf ./temp
mkdir temp

echo "#h numthreads realtime cputime" > RESULTS.txt 

wsc.new run_3phases_MULTINODE.ws -t -dot -dbg
#mv query.exe onethread.

for threads in `seq 1 2`; do
  export NUMTHREADS=$threads

if [ ! true ]; then
  #ramp_up_rate.sh temp/log 5 10 500 ./query.exe -j $threads -n 200 
  ramp_up_rate.sh temp/log 10 20 200 ./query.exe -j $threads -n 30 
  echo "# The above was for $threads threads." >> RESULTS.txt
  mv RESULTS.txt "./temp/RESULTS_"$threads".txt"
else

  time ./query.exe -j $threads -n 1 &> temp/LOG_$threads
  export TMP=`$REGIMENTD/bin/extract_user.sh temp/LOG_$threads`
  export TMP2=`$REGIMENTD/bin/extract_real.sh temp/LOG_$threads`
  echo "$threads  $TMP  $TMP2" >> RESULTS.txt
#  mv RESULTS.txt "./temp/RESULTS_"$threads".txt"

fi

done
