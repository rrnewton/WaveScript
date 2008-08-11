#!/bin/sh

TEMP="./logs/"
START=`pwd`

source ../shared.sh

ONE=/tmp/one_`date +%s`
TWO=/tmp/two_`date +%s`
DUMMYFILE=/tmp/dummy_`date +%s`_.bin

echo " Building a big binary file in /tmp/ to use as input..."
# This makes a 500MB file.
rm -f $ONE $TWO
echo "nonsenseseednonsenseseednonsenseseednonsenseseednonsenseseed" > $ONE
for ((i = 1; i <= 17; i++)); do 
  cat $ONE >> $TWO
  cat $TWO >> $ONE
done
mv $ONE $DUMMYFILE
rm -f $TWO

## ================================================================================ ##
echo;echo;echo " *** Running misc microbenchmarks  Takes approx ?? minutes. ***"; echo; 

rm -rf $TEMP
mkdir $TEMP

# cd "$REGIMENTD/apps/marmot/"
# rm -f 6sec_marmot_sample.raw
# ./download_small_sample_data
# cp 6sec_marmot_sample.raw "$START"/
# cd $START
ln -f -s $DUMMYFILE 6sec_marmot_sample.raw

## This is ugly, but for supporting the new C++ backend, this needs to
## know how to limit the work via input tuples or output tuples.
## Thus we give both numbers to "runallbackends".

echo "## User time for each benchmark/backend " > RESULTS.txt
print_results_header

runallbackends just_timer         $TEMP __  1000

#runallbackends readfile_bigwins   $TEMP $((63 * 30))             30
# Read about 500 mb:
runallbackends readfile_bigwins   $TEMP __  500

## [2007.11.04] Strange problems with readfile_smallwins!!!  Disabling for now:
#runallbackends readfile_smallwins $TEMP $((63 * 128 * 4 * 30))   30 

runallbackends printing_lists     $TEMP __ 16000
runallbackends conv_SigsegArr     $TEMP 1  8000
runallbackends fft                $TEMP 1  300

# Time has to be measured differently for this we need to mark the start time.
# This is lame, cancel out the affect by removing the line it adds:
#export MEMOIZE=yes
#runallbackends demo4a_fft         $TEMP 100

#runallbackends "../../demos/wavescope/demo4a_fft.ws" $TEMP 1
# And then add it back in our own way:

mv RESULTS.txt RESULTS_misc.txt

dump_plot_script ./plot_misc.gp RESULTS_misc.txt

# Cleanup by killing that big old file:
rm -f $DUMMYFILE
