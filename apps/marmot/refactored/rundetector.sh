#!/bin/bash

unset HANDOPT_BUILDSPLIT
unset HANDOPT_MAPSPLIT
unset HANDOPT_FFTIFFT

source shared.sh

TMP=./templogs

rm -rf $TMP RESULTS.txt
mkdir $TMP

#echo "#h backend realtime cputime" > RESULTS.txt 

export OMITOLD=true

runallbackends run_detector-slow $TMP 1 40
mv RESULTS.txt RESULTS_FFTIFFT_OFF.txt

export HANDOPT_FFTIFFT=true
runallbackends run_detector-slow $TMP 1 40
mv RESULTS.txt RESULTS_FFTIFFT_ON.txt

print_results_header 
cat RESULTS_FFTIFFT_OFF.txt | sed 's/run_detector-slow/Detector/'         >> RESULTS.txt
cat RESULTS_FFTIFFT_ON.txt  | sed 's/run_detector-slow/DetectorRewrites/' >> RESULTS.txt



# if [ ! true ]; then
#   wsc.new test_marmot2.ws -O3 -dot
#   #ramp_up_rate.sh temp/log 5 10 500 ./query.exe -j $threads -n 200 
#   ramp_up_rate.sh temp/log 10 20 200 ./query.exe -j $threads -n 30 
#   echo "# The above was for $threads threads." >> RESULTS.txt

# else

#   ./query.exe -j $threads -n 30 > temp/LOG_$threads
#   export TMP=`$WAVESCRIPTD/bin/extract_startend_real.sh temp/LOG_$threads`
#   export TMP2=`$WAVESCRIPTD/bin/extract_startend_times.sh temp/LOG_$threads`
#   echo "$threads  $TMP  $TMP2" >> RESULTS.txt
# #  mv RESULTS.txt "./temp/RESULTS_"$threads".txt"
# fi

# done
