#!/bin/bash

TEMP="./logs/"
START=`pwd`

NUMCPUS=`number_of_cpus`
echo Number of physical cpus: $NUMCPUS

#if [ "$NUMCPUS" = "" ]; then echo Usage: first argument is number of physical CPUS; exit 1; fi

export BACKENDS=" c2 "
export WHICHTIME=realtimes

source ../shared.sh

echo;echo;echo " *** Running parallel benchmarks  ***"; echo; 

rm -rf $TEMP
mkdir $TEMP

echo "## User time for each benchmark/backend " > RESULTS.txt
print_results_header


# [2008.09.02] I'm having segfaults with O2 or O3 on this right now:
#export C2OPTLVL="-O1"
export C2OPTLVL=" -O0 "
run_multithreaded array_splitjoin $TEMP 500
unset C2OPTLVL


run_multithreaded passchain_10 $TEMP 300

RESULTS=RESULTS_"$HOSTNAME"_passchain_"$CC".txt
mv RESULTS.txt $RESULTS

#dump_plot_script ./plot_misc.gp RESULTS_misc.txt
cat > plot.gp <<EOF
plot '$RESULTS' using 2:3 w lp
EOF
