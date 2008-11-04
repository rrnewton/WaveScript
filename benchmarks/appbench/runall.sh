#!/bin/bash







## ================================================================================ ##
echo;echo;echo " *** Running all application benchs.  Takes approx ?? minutes. ***"; echo; 

# Graph normalized performance, not actual time:
export NORMALIZE=true

rm -rf benchlogs_marmot benchlogs_bgsub

## ================================================================================ ##
##   MARMOT BENCH
## ================================================================================ ##

ws-benchmark marmot1.bench marmot2.bench marmot3.bench marmot_all.bench -o MARMOT || exit 1

gnuplot MARMOT.gp > MARMOT.eps
ps2pdf MARMOT.eps
mv benchlogs benchlogs_marmot



## ================================================================================ ##
##   OTHER MARMOT CONFIGS (Optimizations)
## ================================================================================ ##

# if [ ! true ]; then
# cd "$REGIMENTD/apps/marmot/refactored";
# echo "## Running marmot phase 1&2 with no split AML. " > RESULTS.txt
# runallbackends run_marmot2-maps $TEMP 0 3
# cd "$START"
# mv "$REGIMENTD/apps/marmot/refactored/RESULTS.txt" ./aml_nosplit.txt

# cd "$REGIMENTD/apps/marmot/refactored";
# export HANDOPT_BUILDSPLIT=true
# echo "## Running marmot phase 1&2 with 2-way split AML. " > RESULTS.txt
# runallbackends run_marmot2-maps $TEMP 0 3
# unset HANDOPT_BUILDSPLIT
# cd "$START"
# mv "$REGIMENTD/apps/marmot/refactored/RESULTS.txt" ./aml_datapar.txt
# fi


## ================================================================================ ##
##   STOCKTICKS
## ================================================================================ ##

## (NEEDS TO BE FIXED UNDER C++)
## MLton has no hash table support
export OMITMLTON="true"
## TODO
unset OMITMLTON

## ================================================================================ ##
##   PIPELINE
## ================================================================================ ##

# cd "$REGIMENTD/apps/pipeline/";
# echo "## Running pipeline   " > RESULTS.txt
# runallbackends pipeline $TEMP 0 7000
# cd "$START"
# mv "$REGIMENTD/apps/pipeline/RESULTS.txt" ./pipeline.dat


## ================================================================================ ##
##   POTHOLES
## ================================================================================ ##


# cd "$REGIMENTD/apps/potholes";
# echo "## Running pothole   " > RESULTS.txt
# runallbackends pothole_custom $TEMP 0 1000
# cd "$START"
# mv "$REGIMENTD/apps/potholes/RESULTS.txt" ./pothole.dat


## ================================================================================ ##
##   MFCC (telos_audio)
## ================================================================================ ##


## ================================================================================ ##
##   EEG
## ================================================================================ ##

## ================================================================================ ##
##   UCLA Computer Vision
## ================================================================================ ##

ws-benchmark bgsub3.bench -o BGSUB || exit 1

gnuplot BGSUB.gp > BGSUB.eps
ps2pdf BGSUB.eps
mv benchlogs benchlogs_bgsub

## ================================================================================ ##
## APPEND RESULTS:
## ================================================================================ ##

# #cat aml_datapar.txt >> RESULTS.txt
# #cat aml_nosplit.txt >> RESULTS.txt

# #cat pipeline.dat >> RESULTS.txt
# #cat pothole.dat >> RESULTS.txt

