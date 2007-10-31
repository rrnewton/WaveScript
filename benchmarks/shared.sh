

WSCARGS="-j 1 --at_once"

function print_results_header() {
  echo "Benchmark Scheme \"Scheme -O3\" \"XStream $WSCARGS\" MLton" >> RESULTS.txt
}

function runallbackends() {
  NAME=$1
  FILE=$1.ws
  DEST=$2
  TUPS=$3
  echo;echo "Running $FILE";


  echo "  scheme: running... -n $TUPS"
  ws $FILE -exit-error -n $TUPS -t &> $DEST/scheme.$NAME.out


  echo "  scheme -O3: running... -n $TUPS"
  ws.opt $FILE -O3 -exit-error -n $TUPS -t &> $DEST/schemeO3.$NAME.out


  echo "  mlton: compiling..."
  wsmlton $FILE -exit-error  &> $DEST/mlton.compile.$NAME.out
  echo "   mlton: running... -n "$TUPS
#  (/usr/bin/time -f "usertime %U\nrealtime %e\n" ./query.mlton.exe -n $TUPS) &> $DEST/mlton.$NAME.out
  (time ./query.mlton.exe -n $TUPS) &> $DEST/mlton.$NAME.out


  echo "  cpp: compiling..."
  wsc $FILE -t -exit-error   &> $DEST/cpp.compile.$NAME.out
  echo "    cpp: running... -n "$TUPS
  (time ./query.exe $WSCARGS -n $TUPS) &> $DEST/cpp.$NAME.out   
  echo $NAME `extract_scheme_usertimes.sh $DEST/scheme.$NAME.out` `extract_scheme_usertimes.sh $DEST/schemeO3.$NAME.out`  `extract_cpp_usertimes.sh $DEST/cpp.$NAME.out` `extract_mlton_usertimes.sh $DEST/mlton.$NAME.out` >> RESULTS.txt
}
