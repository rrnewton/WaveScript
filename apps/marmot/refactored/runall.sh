#!/bin/bash

export HANDOPT_BUILDSPLIT=true

rm -rf ./temp
mkdir temp

for threads in `seq 1 16`; do
  export NUMTHREADS=$threads
  wsc.new test_marmot2.ws -O3
#  ramp_up_rate.sh temp/log 5 10 400 ./query.exe -j $threads -n 30 
  ramp_up_rate.sh temp/log 10 20 200 ./query.exe -j $threads -n 30 
  mv RESULTS.txt "./temp/RESULTS_"$threads".txt"
done
