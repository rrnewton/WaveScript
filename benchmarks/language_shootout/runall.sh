#!/bin/sh

TEMP="./logs/"

source ../shared.sh

## ================================================================================ ##
echo;echo;echo " *** Running all language-shootout benchmarks.  Takes approx ?? minutes. ***"; echo; 

rm -rf $TEMP
mkdir $TEMP

echo "## User time for each language-shootout benchmark/backend " > RESULTS.txt
echo "Shootout ChezScheme GCC MLton" >> RESULTS.txt
runallbackends fannkuch2   $TEMP 1

#echo $NAME `extract_scheme_usertimes.sh $DEST/scheme.$NAME.out` `extract_cpp_usertimes.sh $DEST/cpp.$NAME.out` `extract_mlton_usertimes.sh $DEST/mlton.$NAME.out` >> RESULTS.txt

grep "CPU ticks" $DEST/scheme.$NAME.out
grep "CPU ticks" $DEST/cpp.$NAME.out
grep "CPU ticks" $DEST/mlton.$NAME.out
