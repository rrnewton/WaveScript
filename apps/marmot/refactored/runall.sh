#!/bin/bash

export HANDOPT_BUILDSPLIT=true

rm -rf ./temp
mkdir temp

for threads in `seq 2 16`; do
  export NUMTHREADS=$threads
#  export NUMTHREADS=16
#  wsc.new test_marmot2.ws -O3

#  ramp_up_rate.sh temp/log 5 10 500 ./query.exe -j $threads -n 200 

  ramp_up_rate.sh temp/log 10 20 200 ./query.exe -j $threads -n 30 
  echo "# The above was for $threads threads." >> RESULTS.txt
  mv RESULTS.txt "./temp/RESULTS_"$threads".txt"
done
