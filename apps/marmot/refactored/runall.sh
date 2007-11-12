#!/bin/bash

unset HANDOPT_BUILDSPLIT
unset HANDOPT_MAPSPLIT

#export HANDOPT_BUILDSPLIT=true
export HANDOPT_MAPSPLIT=true

rm -rf ./temp
mkdir temp

echo "#h numthreads realtime cputime" > RESULTS.txt 

for threads in `seq 1 16`; do
  export NUMTHREADS=$threads
#  export NUMTHREADS=16

if [ ! true ]; then
  wsc.new test_marmot2.ws -O3 -dot
  #ramp_up_rate.sh temp/log 5 10 500 ./query.exe -j $threads -n 200 
  ramp_up_rate.sh temp/log 10 20 200 ./query.exe -j $threads -n 30 
  echo "# The above was for $threads threads." >> RESULTS.txt

  mv RESULTS.txt "./temp/RESULTS_"$threads".txt"

else
  wsc.new test_marmot2.ws -O3 -t -dot
  ./query.exe -j $threads -n 30 > temp/LOG_$threads
  export TMP=`$REGIMENTD/bin/extract_startend_real.sh temp/LOG_$threads`
  export TMP2=`$REGIMENTD/bin/extract_startend_times.sh temp/LOG_$threads`
  echo "$threads  $TMP  $TMP2" >> RESULTS.txt
#  mv RESULTS.txt "./temp/RESULTS_"$threads".txt"
fi

done
