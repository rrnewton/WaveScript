#!/bin/sh

TEMP="./temp/"

WSCARGS="-j 1 --at_once"

function doall() {
  NAME=$1
  FILE=$1.ws
  TUPS=$2
  echo;echo "Running $FILE";
  echo "  scheme: running... -n $TUPS"
  ws $FILE -n $TUPS -t &> $TEMP/scheme.$NAME.out
  echo "  mlton: compiling..."
  wsmlton $FILE &> $TEMP/mlton.compile.$NAME.out
  echo "   mlton: running... -n "$TUPS
  (/usr/bin/time -f "usertime %S\nrealtime %e\n" ./query.mlton.exe -n $TUPS) &> $TEMP/mlton.$NAME.out
  echo "  cpp: compiling..."
  wsc $FILE -t &> $TEMP/cpp.compile.$NAME.out
  echo "    cpp: running... -n "$TUPS
  (time ./query.exe $WSCARGS -n $TUPS) &> $TEMP/cpp.$NAME.out   
  echo $NAME `extract_scheme_usertimes.sh $TEMP/scheme.$NAME.out` `extract_cpp_usertimes.sh $TEMP/cpp.$NAME.out` `extract_mlton_usertimes.sh $TEMP/mlton.$NAME.out` >> RESULTS.txt
}


## ================================================================================ ##
echo;echo " *** Running all microbenchmarks, building pdf summary.  Takes approx ?? minutes. ***"; echo; echo;

rm -rf $TEMP
mkdir $TEMP

doall read_filedata_bigwins 15
doall read_filedata_smallwins 15
doall just_timer 50

# echo "TIMES EXTRACTED:"
# extract_scheme_usertimes.sh $TEMP/scheme.out
# extract_mlton_usertimes.sh $TEMP/mlton.out 
# extract_cpp_usertimes.sh $TEMP/cpp.out 

