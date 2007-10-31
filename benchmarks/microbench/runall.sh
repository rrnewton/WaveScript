#!/bin/sh

TEMP="./logs/"
START=`pwd`

source ../shared.sh

## ================================================================================ ##
echo;echo;echo " *** Running all microbenchmarks  Takes approx 5-10 minutes. ***"; echo; 

rm -rf $TEMP
mkdir $TEMP

# cd "$REGIMENTD/apps/marmot/"
# rm -f 6sec_marmot_sample.raw
# ./download_small_sample_data
# cp 6sec_marmot_sample.raw "$START"/
# cd $START
ln -s /tmp/dummyfile.bin 6sec_marmot_sample.raw

echo "## User time for each benchmark/backend " > RESULTS.txt
print_results_header
runallbackends readfile_bigwins   $TEMP 30 
runallbackends readfile_smallwins $TEMP 30 
runallbackends just_timer         $TEMP 35 
runallbackends printing_lists     $TEMP 8000
runallbackends conv_SigsegArr     $TEMP 20
runallbackends fft                $TEMP 100

# Time has to be measured differently for this we need to mark the start time.
# This is lame, cancel out the affect by removing the line it adds:
#export MEMOIZE=yes
#runallbackends demo4a_fft         $TEMP 100

#runallbackends "../../demos/wavescope/demo4a_fft.ws" $TEMP 1
# And then add it back in our own way:
