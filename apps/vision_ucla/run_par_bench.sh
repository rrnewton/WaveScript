#!/bin/bash

TEMP="./logs/"
START=`pwd`

export NUMCPUS=`number_of_cpus`
echo Number of physical cpus: $NUMCPUS
export BACKENDS=" c2 "
export WHICHTIME=realtimes
source $WAVESCRIPTD/benchmarks/shared.sh
echo;echo;echo " *** Running parallel benchmarks  ***"; echo; 
rm -rf $TEMP
mkdir $TEMP
echo "## Time for each benchmark/backend " > RESULTS.txt
print_results_header


run_multithreaded bgSub4_patchoriented $TEMP 10

mv RESULTS.txt RESULTS_"$HOSTNAME"_"$CC".txt

#dump_plot_script ./plot_misc.gp RESULTS_misc.txt

