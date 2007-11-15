#!/bin/sh


START=`pwd`
TEMP="$START/logs/"

source ../shared.sh

## ================================================================================ ##
echo;echo;echo " *** Running all application benchs.  Takes approx ?? minutes. ***"; echo; 

rm -rf $TEMP
mkdir $TEMP


## ================================================================================ ##
##   MARMOT BENCH
## ================================================================================ ##

function getfile() {
  echo "  Making a big enough audio file."; echo
  cd "$REGIMENTD/apps/marmot";
 (rm -f 6sec_marmot_sample.raw)
  (./download_small_sample_data)
  (cp 6sec_marmot_sample.raw temp.raw;)
  (cat temp.raw >> 6sec_marmot_sample.raw)
  (cat temp.raw >> 6sec_marmot_sample.raw)
  (cat temp.raw >> 6sec_marmot_sample.raw)
  (cat temp.raw >> 6sec_marmot_sample.raw)
  (cat temp.raw >> 6sec_marmot_sample.raw)
  (cat temp.raw >> 6sec_marmot_sample.raw)
}

#getfile

echo "## Running original marmot app. " > RESULTS.txt
#print_results_header
#runallbackends run_first_phase $TEMP 1 5

cd "$START"
mv "$REGIMENTD/apps/marmot/RESULTS.txt" ./

cp RESULTS.txt marmot.txt
echo '\begin{verbatim}' > marmot.tex
cat marmot.txt         >> marmot.tex
echo '\end{verbatim}'  >> marmot.tex


## ================================================================================ ##
##   OTHER MARMOT CONFIGS (Optimizations)
## ================================================================================ ##

if [ ! true ]; then
cd "$REGIMENTD/apps/marmot/refactored";
echo "## Running marmot phase 1&2 with no split AML. " > RESULTS.txt
runallbackends run_marmot2-maps $TEMP 0 3
cd "$START"
mv "$REGIMENTD/apps/marmot/refactored/RESULTS.txt" ./aml_nosplit.txt

cd "$REGIMENTD/apps/marmot/refactored";
export HANDOPT_BUILDSPLIT=true
echo "## Running marmot phase 1&2 with 2-way split AML. " > RESULTS.txt
runallbackends run_marmot2-maps $TEMP 0 3
unset HANDOPT_BUILDSPLIT
cd "$START"
mv "$REGIMENTD/apps/marmot/refactored/RESULTS.txt" ./aml_datapar.txt
fi


cd "$REGIMENTD/apps/marmot/";
echo "## Running orig marmot phase 1  " > RESULTS.txt
#runallbackends run_first_phase $TEMP 0 4
cd "$START"
mv "$REGIMENTD/apps/marmot/RESULTS.txt" ./marmot1.dat

cd "$REGIMENTD/apps/marmot/";
echo "## Running marmot2  " > RESULTS.txt
#runallbackends test_marmot2 $TEMP 0 30
cd "$START"
mv "$REGIMENTD/apps/marmot/RESULTS.txt" ./marmot2.dat

cd "$REGIMENTD/apps/marmot/";
echo "## Running marmot3  " > RESULTS.txt
#runallbackends test_heatmap $TEMP 0 7
cd "$START"
mv "$REGIMENTD/apps/marmot/RESULTS.txt" ./marmot2.dat

cd "$REGIMENTD/apps/marmot/";
echo "## Running marmot multinode offline  " > RESULTS.txt
runallbackends run_3phases_MULTINODE $TEMP 0 3
cd "$START"
mv "$REGIMENTD/apps/marmot/RESULTS.txt" ./marmot2.dat







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

## TODO



## ================================================================================ ##
##   POTHOLES
## ================================================================================ ##

## TODO


## APPEND RESULTS:
rm -f RESULTS.txt
print_results_header 
#cat aml_datapar.txt >> RESULTS.txt
#cat aml_nosplit.txt >> RESULTS.txt
cat marmot1.dat >> RESULTS.txt
cat marmot2.dat >> RESULTS.txt
