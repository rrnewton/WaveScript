#!/bin/bash

TEMP="./logs/"
START=`pwd`

NUMCPUS=`number_of_cpus`

#if [ "$NUMCPUS" = "" ]; then echo Usage: first argument is number of physical CPUS; exit 1; fi

export C2OPTIONS=" -threads "
export BACKENDS=" c2 "
export WHICHTIME=realtimes

source ../shared.sh

echo;echo;echo " *** Running parallel benchmarks  ***"; echo; 

rm -rf $TEMP
mkdir $TEMP


echo "## User time for each benchmark/backend " > RESULTS.txt
print_results_header


for ((lim = 1; lim <= $NUMCPUS; lim++)) do
  export LIMITCPUS=$lim;
  echo;echo "RUNNING WITH $LIMITCPUS CPU(S)."
  runallbackends array_splitjoin  $TEMP __ 200
done

for ((lim = 1; lim <= $NUMCPUS; lim++)) do
  export LIMITCPUS=$lim;
  echo;echo "RUNNING WITH $LIMITCPUS CPU(S)."
  runallbackends passchain_10  $TEMP __ 150
done


mv RESULTS.txt RESULTS_"$HOSTNAME"_passchain_"$CC".txt

#dump_plot_script ./plot_misc.gp RESULTS_misc.txt

# Cleanup by killing that big old file:
rm -f $DUMMYFILE
