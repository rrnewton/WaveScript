#!/bin/sh

TEMP="./logs/"
START=`pwd`

source ../shared.sh

## ================================================================================ ##
echo;echo;echo " *** Running misc microbenchmarks  Takes approx ?? minutes. ***"; echo; 

rm -rf $TEMP
mkdir $TEMP

# cd "$REGIMENTD/apps/marmot/"
# rm -f 6sec_marmot_sample.raw
# ./download_small_sample_data
# cp 6sec_marmot_sample.raw "$START"/
# cd $START
ln -f -s /tmp/dummyfile.bin 6sec_marmot_sample.raw

## This is ugly, but for supporting the new C++ backend, this needs to
## know how to limit the work via input tuples or output tuples.
## Thus we give both numbers to "runallbackends".

echo "## User time for each benchmark/backend " > RESULTS.txt
print_results_header
runallbackends readfile_bigwins   $TEMP $((63 * 30))             30 

## [2007.11.04] Strange problems with readfile_smallwins!!!  Disabling for now:
#runallbackends readfile_smallwins $TEMP $((63 * 128 * 4 * 30))   30 


#runallbackends just_timer         $TEMP 1  35 
runallbackends printing_lists     $TEMP 1  8000
runallbackends conv_SigsegArr     $TEMP 1  5
runallbackends fft                $TEMP 1  100

# Time has to be measured differently for this we need to mark the start time.
# This is lame, cancel out the affect by removing the line it adds:
#export MEMOIZE=yes
#runallbackends demo4a_fft         $TEMP 100

#runallbackends "../../demos/wavescope/demo4a_fft.ws" $TEMP 1
# And then add it back in our own way:

mv RESULTS.txt RESULTS_misc.txt
