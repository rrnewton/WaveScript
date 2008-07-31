#!/bin/sh

TEMP="./logs/"
START=`pwd`

source ../shared.sh

## ================================================================================ ##
echo;echo;echo " *** Running datapass microbenchmarks  ***"; echo; 

rm -rf $TEMP
mkdir $TEMP

ln -f -s /tmp/dummyfile.bin 6sec_marmot_sample.raw

echo "## User time for each benchmark/backend " > RESULTS.txt
print_results_header

runallbackends pass_raw       $TEMP __  300000
runallbackends pass_static    $TEMP __  300000
runallbackends pass_arrays    $TEMP __  300000
runallbackends pass_lists     $TEMP __  300000

# Time has to be measured differently for this we need to mark the start time.
# This is lame, cancel out the affect by removing the line it adds:
#export MEMOIZE=yes
#runallbackends demo4a_fft         $TEMP 100

#runallbackends "../../demos/wavescope/demo4a_fft.ws" $TEMP 1
# And then add it back in our own way:

mv RESULTS.txt RESULTS_datapass.txt

dump_plot_script ./plot_datapass.gp RESULTS_datapass.txt
