#!/bin/bash


#START=`pwd`
#TEMP="$START/logs/"

#source ../shared.sh

## ================================================================================ ##
echo;echo;echo " *** Running all application benchs.  Takes approx ?? minutes. ***"; echo; 

#rm -rf $TEMP
#mkdir $TEMP

#rm -f RESULTS.txt
#print_results_header 

## ================================================================================ ##
##   MARMOT BENCH
## ================================================================================ ##

ws-benchmark marmot1.bench marmot2.bench marmot3.bench marmot_all.bench -o RESULTS

mv RESULTS.gp plot.gp

#export REGIMENTHOST=ikarus

# cd "$REGIMENTD/apps/marmot/";
# echo "## Running orig marmot phase 1  " > RESULTS.txt
# runallbackends run_first_phase $TEMP __ 100
# cd "$START"
# mv "$REGIMENTD/apps/marmot/RESULTS.txt" ./marmot1.dat
# p
# cd "$REGIMENTD/apps/marmot/";
# echo "## Running marmot2  " > RESULTS.txt
# runallbackends test_marmot2 $TEMP __ 150
# cd "$START"
# mv "$REGIMENTD/apps/marmot/RESULTS.txt" ./marmot2.dat

# #export REGIMENTHOST=plt

# cd "$REGIMENTD/apps/marmot/";
# echo "## Running marmot3  " > RESULTS.txt
# runallbackends test_heatmap $TEMP __ 14
# cd "$START"
# mv "$REGIMENTD/apps/marmot/RESULTS.txt" ./marmot3.dat

# cd "$REGIMENTD/apps/marmot/";
# echo "## Running marmot multinode offline  " > RESULTS.txt
# #runallbackends run_3phases_MULTINODE $TEMP 0 3
# runallbackends run_3phases $TEMP __ 20
# cd "$START"
# mv "$REGIMENTD/apps/marmot/RESULTS.txt" ./marmot_multi.dat

# cat marmot1.dat marmot2.dat marmot3.dat cat marmot_multi.dat >> RESULTS.txt

# cp RESULTS.txt marmot.txt
# echo '\begin{verbatim}' > marmot.tex
# cat marmot.txt         >> marmot.tex
# echo '\end{verbatim}'  >> marmot.tex



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

#cd "$REGIMENTD/apps/vision_ucla/";
#./download_small_sample_data
#runallbackends bgSub $TEMP __ 10
#runallbackends bgSub3_integer $TEMP __ 10
#runallbackends bgSub4_patchoriented $TEMP __ 10
#cd "$START"
#mv "$REGIMENTD/apps/marmot/RESULTS.txt" ./marmot2.dat

## ================================================================================ ##
## APPEND RESULTS:
## ================================================================================ ##

# #cat aml_datapar.txt >> RESULTS.txt
# #cat aml_nosplit.txt >> RESULTS.txt

# #cat pipeline.dat >> RESULTS.txt
# #cat pothole.dat >> RESULTS.txt



# echo '\begin{verbatim}' > RESULTS.tex
# cat RESULTS.txt         >> RESULTS.tex
# echo '\end{verbatim}'  >> RESULTS.tex

# dump_plot_script ./plot.gp RESULTS.txt

# rm -f $TMPDAT
