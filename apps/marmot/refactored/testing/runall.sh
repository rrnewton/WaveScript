#!/bin/bash


rm -rf ./temp query.*
mkdir temp

echo "#h numthreads realtime cputime" > RESULTS.txt 

export NUMTHREADS=16
export NUMBOXES=15
wsc.new embarassing.ws -O3 -t -dot

for threads in `seq 1 16`; do
  export NUMTHREADS=$threads
  #export NUMBOXES=$(($threads - 1))
#  export NUMBOXES=15
#  wsc.new embarassing.ws -O3 -t -dot

  echo Running for $threads threads...
  (time ./query.exe -j $threads -n 1600) 1> "temp/LOG_$threads".out 2> "temp/LOG_$threads".err
  export TMP=`$REGIMENTD/bin/extract_real.sh temp/LOG_"$threads".err`
  export TMP2=`$REGIMENTD/bin/extract_user.sh temp/LOG_"$threads".err`

  #INT=`echo $TMP | sed 's/\.0//'`
  #echo INTEGER $INT
  
  echo "$threads  $TMP  $TMP2" >> RESULTS.txt
#  mv RESULTS.txt "./temp/RESULTS_"$threads".txt"
done
