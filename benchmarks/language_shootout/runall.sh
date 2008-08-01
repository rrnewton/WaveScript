#!/bin/sh

TEMP="./logs/"

source ../shared.sh

#BACKENDS="camlO3 $BACKENDS"
BACKENDS="c2"

## ================================================================================ ##
echo;echo;echo " *** Running all language-shootout benchmarks.  Takes approx ?? minutes. ***"; echo; 

rm -rf $TEMP
mkdir $TEMP

echo "## User time for each language-shootout benchmark/backend " > RESULTS.txt

print_results_header
runallbackends fannkuch2   $TEMP 1 1

#grep "CPU ticks" $DEST/scheme.$NAME.out
#grep "CPU ticks" $DEST/cpp.$NAME.out
#grep "CPU ticks" $DEST/mlton.$NAME.out

rm -f plot.gp
dump_plot_script ./plot.gp RESULTS.txt
echo Finished.
cat plot.gp
ls -l plot.gp
pwd -P
