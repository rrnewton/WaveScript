#!/bin/bash

TEMP="./logs/"
START=`pwd`

source ../shared.sh

## ================================================================================ ##
echo;echo;echo " *** Running all matrix benchmarks  Takes approx 5-10 minutes. ***"; echo; 

rm -rf $TEMP
mkdir $TEMP

## This is ugly, but for supporting the new C++ backend, this needs to
## know how to limit the work via input tuples or output tuples.
## Thus we give both numbers to "runallbackends".

echo "## User time for each benchmark/backend " > RESULTS.txt
print_results_header
runallbackends matrix_benchmark   $TEMP 1 1
runallbackends matrix_benchmark_rowmajor_spec   $TEMP 1 1
runallbackends matrix_benchmark_rowmajor   $TEMP 1 1
./a.out | ../extract_startend_times.sh >> RESULTS.txt

