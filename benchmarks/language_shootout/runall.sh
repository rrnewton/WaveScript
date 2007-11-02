#!/bin/sh

TEMP="./logs/"

source ../shared.sh

## ================================================================================ ##
echo;echo;echo " *** Running all language-shootout benchmarks.  Takes approx ?? minutes. ***"; echo; 

rm -rf $TEMP
mkdir $TEMP

echo "## User time for each language-shootout benchmark/backend " > RESULTS.txt
print_results_header
runallbackends fannkuch2   $TEMP 1 1

grep "CPU ticks" $DEST/scheme.$NAME.out
grep "CPU ticks" $DEST/cpp.$NAME.out
grep "CPU ticks" $DEST/mlton.$NAME.out
