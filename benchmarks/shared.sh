

OLDWSCARGS="-j 1 --at_once"
WSCARGS="-j 1"

function print_results_header() {
  echo "Benchmark \"Scheme -O2\" \"Scheme -O3\" \"XStream $WSCARGS\" \"XStream DepthFirst\" \"CoreFit DF\" \"MLton -O2\"" >> RESULTS.txt
}

function runallbackends() {
  NAME=$1
  FILE=$1.ws
  DEST=$2
  INTUPS=$3
  TUPS=$4
  echo;echo "Running $FILE, INTUPS: $INTUPS  OUTTUPS: $TUPS";

  # Clean up:
  rm -f query.*  



  echo "  scheme: running... -n $TUPS"
  ws $FILE -exit-error -n $TUPS -t &> $DEST/scheme.$NAME.out

  echo "  scheme -O3: running... -n $TUPS"
  ws.opt $FILE -O3 -exit-error -n $TUPS -t &> $DEST/schemeO3.$NAME.out


  echo "  mlton: compiling..."
  wsmlton $FILE -exit-error  &> $DEST/mlton.compile.$NAME.out
  echo "   mlton: running... -n "$TUPS
#  (/usr/bin/time -f "usertime %U\nrealtime %e\n" ./query.mlton.exe -n $TUPS) &> $DEST/mlton.$NAME.out
  (time ./query.mlton.exe -n $TUPS) &> $DEST/mlton.$NAME.out


  # ================================================================================

  ## FIRST THE OLD SCHEDULER:  
  ## ----------------------------------------
  export WAVESCOPED="$REGIMENTD/benchmarks/engine/1495"

  echo "  cpp: compiling..."
  rm -f "$WAVESCOPED/libws-SMSegList.a" 
  ln -s "$REGIMENTD/benchmarks/libws-SMSegList.1495.O2.default.a" "$WAVESCOPED/libws-SMSegList.a" 
  if wsc $FILE -t -exit-error   &> $DEST/cpp.compile.$NAME.out; then echo>/dev/null;
  else echo "wsc failed!"; exit -1; fi
  echo "    cpp: running... -n $TUPS $OLDWSCARGS"
  (time ./query.exe $OLDWSCARGS -n $TUPS) &> $DEST/cpp.$NAME.out   
  rm -f query.*  

  echo "  cpp: -DDEPTH_FIRST compiling..."
  # MUST PASS THIS TO G++:
  rm -f "$WAVESCOPED/libws-SMSegList.a" 
  ln -s "$REGIMENTD/benchmarks/libws-SMSegList.1495.O2.df.a" "$WAVESCOPED/libws-SMSegList.a" 
  # This didn't work:
  #ln -s "$REGIMENTD/benchmarks/libws-SMSegList.1495.O2.traindf.a" "$WAVESCOPED/libws-SMSegList.a" 
  #echo "  cpp: -DTRAIN_SCHEDULER -DDEPTH_FIRST compiling..."
  if  wsc $FILE -t -exit-error --scheduler depth-first &> $DEST/cppdf.compile.$NAME.out; then echo>/dev/null;
  else echo "wsc failed!"; exit -1; fi
  echo "    cpp: running... -n $TUPS $OLDWSCARGS"
  (time ./query.exe $OLDWSCARGS -n $TUPS) &> $DEST/cppdf.$NAME.out   
  rm -f query.*  

  ## NOW THE NEW SCHEDULER:
  ## ----------------------------------------
  export WAVESCOPED="$REGIMENTD/benchmarks/engine/newest"

  echo "  cpp: COREFIT_SCHEDULER_DF compiling..."
  # MUST PASS THIS TO G++:
  rm -f "$WAVESCOPED/libws-SMSegList.a" 
  ln -s "$REGIMENTD/benchmarks/libws-SMSegList.newest.O2.coredf.a" "$WAVESCOPED/libws-SMSegList.a" 
  # This didn't work:
  #ln -s "$REGIMENTD/benchmarks/libws-SMSegList.1495.O2.traindf.a" "$WAVESCOPED/libws-SMSegList.a" 
  #echo "  cpp: -DTRAIN_SCHEDULER -DDEPTH_FIRST compiling..."
  if  wsc $FILE --scheduler corefit-scheduler-df -exit-error  &> $DEST/cppnew.compile.$NAME.out; then echo>/dev/null;
  else echo "wsc failed!"; exit -1; fi
  echo "    cpp: running... -n $TUPS $WSCARGS"
  (time ./query.exe $WSCARGS -n $TUPS) &> $DEST/cppnew.$NAME.out   
  rm -f query.*  

  # ================================================================================


#  export WAVESCOPED="$REGIMENTD/benchmarks/engine/newest"


  # ================================================================================
  echo $NAME `extract_scheme_usertimes.sh $DEST/scheme.$NAME.out`  \
             `extract_scheme_usertimes.sh $DEST/schemeO3.$NAME.out` \
             `extract_mlton_usertimes.sh $DEST/cpp.$NAME.out`          \
             `extract_mlton_usertimes.sh $DEST/cppdf.$NAME.out`         \
             `extract_mlton_usertimes.sh $DEST/cppnew.$NAME.out`         \
             `extract_mlton_usertimes.sh $DEST/mlton.$NAME.out`       >> RESULTS.txt

}
