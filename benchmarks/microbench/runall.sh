#!/bin/sh

TEMP="./logs/"

WSCARGS="-j 1 --at_once"

function doall() {
  NAME=$1
  FILE=$1.ws
  DEST=$2
  TUPS=$3
  echo;echo "Running $FILE";
  echo "  scheme: running... -n $TUPS"
  ws $FILE -n $TUPS -t &> $DEST/scheme.$NAME.out
  echo "  mlton: compiling..."
  wsmlton $FILE &> $DEST/mlton.compile.$NAME.out
  echo "   mlton: running... -n "$TUPS
  (/usr/bin/time -f "usertime %S\nrealtime %e\n" ./query.mlton.exe -n $TUPS) &> $DEST/mlton.$NAME.out
  echo "  cpp: compiling..."
  wsc $FILE -t &> $DEST/cpp.compile.$NAME.out
  echo "    cpp: running... -n "$TUPS
  (time ./query.exe $WSCARGS -n $TUPS) &> $DEST/cpp.$NAME.out   
  echo $NAME `extract_scheme_usertimes.sh $DEST/scheme.$NAME.out` `extract_cpp_usertimes.sh $DEST/cpp.$NAME.out` `extract_mlton_usertimes.sh $DEST/mlton.$NAME.out` >> RESULTS.txt
}


## ================================================================================ ##
echo;echo " *** Running all microbenchmarks, building pdf summary.  Takes approx ?? minutes. ***"; echo; echo;

rm -rf $TEMP
mkdir $TEMP

echo "## User time for each benchmark/backend " > RESULTS.txt
echo "Benchmark ChezScheme GCC MLton" >> RESULTS.txt
doall readfile_bigwins   $TEMP 30 
doall readfile_smallwins $TEMP 30 
doall just_timer         $TEMP 35 
