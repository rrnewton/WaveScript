#!/bin/bash

TEMP="./logs/"
START=`pwd`

source ../shared.sh

## ================================================================================ ##
echo;echo;echo " *** Running data passing / allocation microbenchmarks  ***"; echo; 

rm -rf $TEMP plain.dat hoard.dat tcmalloc.dat mlton.dat scheme.dat *_manual.dat
mkdir $TEMP

#echo "## User time for each benchmark/backend " > RESULTS.txt
#print_results_header

#runallbackends just_timer         $TEMP 1  35 
#runallbackends edge_stress         $TEMP  1  1

#mv RESULTS.txt RESULTS_datapass.txt

COMPILE=wsc2
RUN=./query.exe


# function manual_compile() {
#    echo "IN MANUAL COMPILE $1"
#    gcc $BENCHMARK
# }

function runit() {
  for i in 5 10 25 50 100 250 500 750 1000 2000 5000 10000; do
    export SCALEFACTOR=$i
    FILE=$TEMP/time_$i.txt  
    echo " Compiling with: $COMPILE $BENCHMARK "
    $COMPILE $BENCHMARK  &> /dev/null
    echo Running with size $i ...
    (time $RUN -n 10) 2> $FILE
    cat $FILE;
    echo "$i "`extract_real.sh $FILE` >> $RESULTS
  done;
}

function runscheme() {
  for i in 5 10 25 50 100 250 500 750 1000 2000 5000 10000; do
    export SCALEFACTOR=$i
    FILE=$TEMP/time_$i.txt  
    echo Running with size $i ...
    ws.opt $BENCHMARK -n 10 -t &> $FILE
    cat $FILE
    echo "$i" `extract_scheme_realtimes.sh $FILE` >> $RESULTS
  done;
}


export LD_PRELOAD= 

function runeverybody() {

  echo;echo RUNNING WITH BUILTIN MALLOC.
  RESULTS=plain.dat
  export LD_PRELOAD= 
  runit

  echo;echo RUNNING WITH HOARD.
  RESULTS=hoard.dat
  #OPTIONS=-lhoard
  export LD_PRELOAD=/usr/lib/libhoard.so
  runit

  echo;echo RUNNING WITH TCMALLOC.
  RESULTS=tcmalloc.dat
  #OPTIONS=-lptmalloc3
  #OPTIONS=-ltcmalloc
  export WS_LINK="-lpthread -ltcmalloc"
  #export LD_PRELOAD=/usr/lib/libtcmalloc.so
  export LD_PRELOAD=
  runit

  # echo;echo RUNNING WITH NEDMALLOC.
  # RESULTS=nedmalloc.dat
  #WS_LINK="-lpthread nedmalloc.c"
  # #export LD_PRELOAD=/usr/lib/libtcmalloc.so
  # export LD_PRELOAD=
  # runit

   echo;echo RUNNING WITH MLTON
   RESULTS=mlton.dat
   COMPILE=wsmlton
   RUN=./query.mlton.exe
   runit

   echo;echo RUNNING WITH SCHEME
   RESULTS=scheme.dat
   runscheme
}


function run_manual() {
  #rm -rf manual_results
  #mkdir manual_results

  gcc -O3 $CSOURCE
  RESULTS=plain_$SUFFIX.dat
  COMPILE="echo"
  RUN=./a.out
  export LD_PRELOAD= 
  runit

  export LD_PRELOAD=/usr/lib/libhoard.so
  RESULTS=hoard_$SUFFIX.dat
  runit

  export LD_PRELOAD=
  gcc -O3 -lpthread -ltcmalloc $CSOURCE
  RESULTS=tcmalloc_$SUFFIX.dat
  runit

  #mv *.dat manual_results/
}


########################################
BENCHMARK=datapass_allocarrays.ws
rm -rf 1d_results
mkdir 1d_results
#runeverybody 
mv *.dat 1d_results

########################################
BENCHMARK=datapass_nestedarrays.ws
runeverybody 

########################################
CSOURCE=manual_zct.c
SUFFIX=manual
#run_manual

########################################
CSOURCE=manual_best.c
SUFFIX=best
run_manual

########################################
CSOURCE=static_ideal.c
SUFFIX=ideal
run_manual

