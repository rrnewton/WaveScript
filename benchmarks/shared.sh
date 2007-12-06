

OLDWSCARGS="-j 1 --at_once"
WSCARGS="-j 1"

function print_results_header() {
  echo "Benchmark \"Scheme -O2\" \"Scheme -O3\" \"XStream $OLDWSCARGS\" \"XStream DepthFirst $OLDWSCARGS\" \"CoreFit DF $WSCARGS\" \"CoreFitDF 1Thread $WSCARGS\" \"MLton -O2\" \"MLton -O3\"" >> RESULTS.txt
}



function runscheme() {
  echo "  scheme: running... -n $TUPS"
  if ! (ws $FILE -exit-error -n $TUPS -t &> $DEST/scheme.$NAME.out); 
  then echo "ws failed!"; exit -1; fi
}

function runschemeO3() {
  echo "  scheme -O3: running... -n $TUPS"
  if ws.opt $FILE -O3 -exit-error -n $TUPS -t &> $DEST/schemeO3.$NAME.out; then echo>/dev/null;
  else echo "ws.opt failed!"; exit -1; fi
}

function runcpp() {
  export WAVESCOPED="$REGIMENTD/benchmarks/engine/1495"

  echo "  cpp: compiling..."
  rm -f "$WAVESCOPED/libws-SMSegList.a" 
  ln -s "$REGIMENTD/benchmarks/libws-SMSegList.1495.O2.default.a" "$WAVESCOPED/libws-SMSegList.a" 
  if wsc $FILE -t -exit-error   &> $DEST/cpp.compile.$NAME.out; then echo>/dev/null;
  else echo "wsc failed!"; exit -1; fi
  echo "    cpp: running... -n $TUPS $OLDWSCARGS"
  if ! (time ./query.exe $OLDWSCARGS -n $TUPS) &> $DEST/cpp.$NAME.out; 
  then echo "failed!"; exit -1; fi
  rm -f query.*  
}

function runcpp_df() {
  export WAVESCOPED="$REGIMENTD/benchmarks/engine/1495"
  echo "  cpp: -DDEPTH_FIRST compiling..."
  rm -f "$WAVESCOPED/libws-SMSegList.a" 
  ln -s "$REGIMENTD/benchmarks/libws-SMSegList.1495.O2.df.a" "$WAVESCOPED/libws-SMSegList.a" 
  if  wsc $FILE -t -exit-error --scheduler depth-first &> $DEST/cppdf.compile.$NAME.out; then echo>/dev/null;
  else echo "wsc failed!"; exit -1; fi
  echo "    cpp: running... -n $TUPS $OLDWSCARGS"
  if ! (time ./query.exe $OLDWSCARGS -n $TUPS) &> $DEST/cppdf.$NAME.out;
  then echo "failed!"; exit -1; fi
  rm -f query.*  
}



# [2007.11.14] Now this supports "push-pull" too (-t)
# As long as we're using engine > 1738
function runcpp_corefit() {
  # This is a flag that the code responds to:
  export COREFITBENCH=true
  export WAVESCOPED="$REGIMENTD/benchmarks/engine/newest"
  echo "  cpp: COREFIT_SCHEDULER_DF compiling..."
  # MUST PASS THIS TO G++:
  rm -f "$WAVESCOPED/libws-SMSegList.a" 
  ln -s "$REGIMENTD/benchmarks/libws-SMSegList.newest.O2.coredf.a" "$WAVESCOPED/libws-SMSegList.a" 
  # This didn't work:
  #ln -s "$REGIMENTD/benchmarks/libws-SMSegList.1495.O2.traindf.a" "$WAVESCOPED/libws-SMSegList.a" 
  #echo "  cpp: -DTRAIN_SCHEDULER -DDEPTH_FIRST compiling..."
  if  wsc $FILE -t --scheduler corefit-scheduler-df -exit-error  &> $DEST/cppnew.compile.$NAME.out; then echo>/dev/null;
  else echo "wsc failed!"; exit -1; fi
  echo "    cpp: running... -n $TUPS $WSCARGS"
  if ! (time ./query.exe $WSCARGS -n $TUPS) &> $DEST/cppnew.$NAME.out;
  then echo "failed!"; exit -1; fi
  rm -f query.*  
  unset COREFITBENCH
}

function runcpp_corefit_nothreads() {
  # This is a flag that the code responds to:
  export COREFITBENCH=true
  export WAVESCOPED="$REGIMENTD/benchmarks/engine/newest_nothreads"
  echo "  cppnothread: COREFIT_SCHEDULER_DF single-threaded compiling..."
  # MUST PASS THIS TO G++:
  rm -f "$WAVESCOPED/libws-SMSegList.a" 
  ln -s "$REGIMENTD/benchmarks/libws-SMSegList.newest.O2.coredf.nothreads.a" "$WAVESCOPED/libws-SMSegList.a" 
  if  wsc $FILE -t -nothreads --scheduler corefit-scheduler-df -exit-error  &> $DEST/cppnothreads.compile.$NAME.out; then echo>/dev/null;
  else echo "wsc failed!"; exit -1; fi
  echo "    cppnothreads: running... -n $TUPS $WSCARGS"
  if ! (time ./query.exe $WSCARGS -n $TUPS) &> $DEST/cppnothreads.$NAME.out;
  then echo "failed!"; exit -1; fi
  rm -f query.*  
  unset COREFITBENCH
}


function runmlton() {
  echo "  mlton: compiling..."
  if wsmlton $FILE -exit-error &> $DEST/mlton.compile.$NAME.out; then echo>/dev/null;
  else echo "wsmlton failed!"; exit -1; fi
  echo "   mlton: running... -n "$TUPS
#  (/usr/bin/time -f "usertime %U\nrealtime %e\n" ./query.mlton.exe -n $TUPS) &> $DEST/mlton.$NAME.out
  if ! (time ./query.mlton.exe -n $TUPS) &> $DEST/mlton.$NAME.out; 
  then echo "failed!"; exit -1; fi
}


function runmltonO3() {
  echo "  mlton -O3: compiling..."
  if wsmlton $FILE -O3 -exit-error &> $DEST/mltonO3.compile.$NAME.out; then echo>/dev/null;
  else echo "wsmlton -O3 failed!"; exit -1; fi
  echo "   mlton -O3: running... -n "$TUPS
#  (/usr/bin/time -f "usertime %U\nrealtime %e\n" ./query.mlton.exe -n $TUPS) &> $DEST/mlton.$NAME.out
  if ! (time ./query.mlton.exe -n $TUPS) &> $DEST/mltonO3.$NAME.out; 
  then echo "failed!"; exit -1; fi
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

  ## FIRST THE OLD SCHEDULER:  
  if [ "$OMITOLD" == "" ]; then 
   echo
    runcpp
    runcpp_df
  fi

  ## NOW THE NEW SCHEDULER:
  runcpp_corefit_nothreads
  runcpp_corefit

  runscheme
  runschemeO3
  
  if [ "$OMITMLTON" == "" ]; then 
    runmlton; 
    runmltonO3;
  fi

  SCHEME=`extract_scheme_usertimes.sh $DEST/scheme.$NAME.out`
  SCHEMEO3=`extract_scheme_usertimes.sh $DEST/schemeO3.$NAME.out`
  ML=`extract_mlton_usertimes.sh $DEST/mlton.$NAME.out`
  MLO3=`extract_mlton_usertimes.sh $DEST/mltonO3.$NAME.out`
  CPP=`extract_mlton_usertimes.sh $DEST/cpp.$NAME.out`
  CPPDF=`extract_mlton_usertimes.sh $DEST/cppdf.$NAME.out`
  CPPNEW=`extract_mlton_usertimes.sh $DEST/cppnew.$NAME.out`
  CPPNOTHREADS=`extract_mlton_usertimes.sh $DEST/cppnothreads.$NAME.out`

## Temporarilly disabling some backends:
#  SCHEME="-1"
#  CPPNEW="-1"
#  CPPNOTHREADS=-1
#  MLO3="-1"

  ## This automatically catches it if one of the backends above was disabled. 
  CPP_BROKE=`echo $CPP | grep File`
  CPPDF_BROKE=`echo $CPPDF | grep File`
  CPPNEW_BROKE=`echo $CPPNEW | grep File`

  if [ ! "$CPP_BROKE" == "" ];   then CPP="-1"; fi
  if [ ! "$CPPDF_BROKE" == "" ]; then CPPDF="-1"; fi
  if [ ! "$CPPNEW_BROKE" == "" ]; then CPPNEW="-1"; fi

  # ================================================================================
  echo "RESULTS: (1) $NAME (2) $SCHEME (3) $SCHEMEO3 (4) $CPP (5) $CPPDF (6) $CPPNEW (7) $CPPNOTHREADS (8) $ML (9) $MLO3"
  echo $NAME $SCHEME $SCHEMEO3 $CPP $CPPDF $CPPNEW $CPPNOTHREADS  $ML $MLO3 >> RESULTS.txt

  #echo RESULTS: $NAME $SCHEMEO3 $CPPNOTHREADS $ML 
  #echo $NAME $SCHEMEO3 $CPPNOTHREADS $ML >> RESULTS.txt
}
