#!/bin/bash

TEMP="./logs/"
START=`pwd`

export WSOPTIONS='-noprint'

source ../shared.sh

BACKENDS="camlO3 $BACKENDS"

ELEMENTS=30000000

## ================================================================================ ##
echo;echo;echo " *** Running datapass microbenchmarks  ***"; echo; 

rm -rf $TEMP
mkdir $TEMP

ln -f -s /tmp/dummyfile.bin 6sec_marmot_sample.raw

function do_each() {
#  echo RUNNING FOR $((300000000 /$BUFSIZE)) tuples

  echo "## User time for each benchmark/backend " > RESULTS.txt
  print_results_header
  echo "## NOTE THAT THE LIST VERSION RUNS 10X FEWER TUPLES" >> RESULTS.txt

  runallbackends pass_raw           $TEMP __  $((30000000 * 10 /$BUFSIZE))
  runallbackends pass_static_array  $TEMP __  $((30000000 * 10 /$BUFSIZE))
  runallbackends pass_arrays        $TEMP __  $((30000000 * 10 /$BUFSIZE))
  runallbackends pass_lists         $TEMP __  $((30000000  /$BUFSIZE))
}

export BUFSIZE=100
do_each
mv RESULTS.txt RESULTS_datapass100.txt

export BUFSIZE=10
do_each
mv RESULTS.txt RESULTS_datapass10.txt

export BUFSIZE=1000
do_each
mv RESULTS.txt RESULTS_datapass1000.txt

echo 'set title "300M elements, Buffer size = 10,  note that pass_lists uses 1/10 the elements "' > plot_datapass10.gp
echo 'set title "300M elements, Buffer size = 100,  note that pass_lists uses 1/10 the elements "' > plot_datapass100.gp
echo 'set title "300M elements, Buffer size = 1000,  note that pass_lists uses 1/10 the elements "' > plot_datapass1000.gp

dump_plot_script ./plot_datapass10.gp   RESULTS_datapass10.txt
dump_plot_script ./plot_datapass100.gp  RESULTS_datapass100.txt
dump_plot_script ./plot_datapass1000.gp RESULTS_datapass1000.txt


