
#!/bin/sh

TEMP="./logs/"
START=`pwd`

source ../shared.sh

## ================================================================================ ##
echo;echo;echo " *** Running data passing microbenchmarks  Takes approx ?? minutes. ***"; echo; 

rm -rf $TEMP
mkdir $TEMP

echo "## User time for each benchmark/backend " > RESULTS.txt
print_results_header

#runallbackends just_timer         $TEMP 1  35 
runallbackends edge_stress         $TEMP  1  1

mv RESULTS.txt RESULTS_datapass.txt
