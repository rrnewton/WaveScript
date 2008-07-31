
## This is the list of backends to test, valid symbols are:
#  scheme, schemeO3
#  mlton, mltonO3 
#  cpp, cpp_df, cpp_corefit, cpp_corefit_nothreads -- The old XStream/C++ backend
#  c2 -- the new C backend
#BACKENDS="scheme mlton mltonO3"
if [ "$BACKENDS" = "" ]; 
then BACKENDS="mltonO3 c2boehm c2boehmseglist c2 c2seglist c2def c2defseglist"
fi

OLDWSCARGS="-j 1 --at_once"
WSCARGS="-j 1"

function print_results_header() {
   echo Benchmark $BACKENDS >> RESULTS.txt
#  echo "Benchmark \"Scheme -O2\" \"Scheme -O3\" \"XStream $OLDWSCARGS\" \"XStream DepthFirst $OLDWSCARGS\" \"CoreFit DF $WSCARGS\" \"CoreFitDF 1Thread $WSCARGS\" \"MLton -O2\" \"MLton -O3\"" >> RESULTS.txt
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


MLTONOPTIONS=
function runmlton() {
  echo "  mlton $MLTONOPTIONS: compiling..."
  if wsmlton $FILE $MLTONOPTIONS -exit-error &> $DEST/mlton$1.compile.$NAME.out; then echo>/dev/null;
  else echo "wsmlton failed!"; exit -1; fi
  echo "   mlton: running... -n "$TUPS
  if ! (time ./query.mlton.exe -n $TUPS) &> $DEST/mlton$1.$NAME.out; 
  then echo "failed!"; exit -1; fi
}

C2OPTIONS=
function runc2() {
  echo "  wsc2 $C2OPTIONS : compiling..."
  if wsc2 $FILE $C2OPTIONS -exit-error &> $DEST/c2$1.compile.$NAME.out; then echo>/dev/null;
  else echo "wsc2 failed!"; exit -1; fi
  echo "   wsc2: running output... -n "$TUPS
  if ! (time ./query.exe -n $TUPS) &> $DEST/c2$1.$NAME.out; 
  then echo "failed!"; exit -1; fi
}



## This is where you disable/enable backends.
## Arguments: filename sans extension, output file, in tuple limit, out tuple limit
## Currently in tuple limit is unimplemented.
function runallbackends() {
  NAME=$1
  FILE=$1.ws
  DEST=$2
  INTUPS=$3
  TUPS=$4
  echo;echo "  $FILE -- Running all backends: INTUPS: $INTUPS  OUTTUPS: $TUPS";

  # Clean up:
  rm -rf query.*  
  echo "Invoking the following backends: $BACKENDS"

  TIMES=
  for backend in $BACKENDS; do
    case $backend in
      scheme)   runscheme;   TIMES+=" "`extract_scheme_usertimes.sh $DEST/scheme.$NAME.out`;;
      schemeO3) runschemeO3; TIMES+=" "`extract_scheme_usertimes.sh $DEST/schemeO3.$NAME.out`;;
      mlton)    runmlton;    TIMES+=" "`extract_mlton_usertimes.sh $DEST/mlton.$NAME.out`;;
      mltonO3)  MLTONOPTIONS="-O3"; 
                runmlton O3; TIMES+=" "`extract_mlton_usertimes.sh $DEST/mltonO3.$NAME.out`;;
      cpp)      runcpp;      TIMES+=" "`extract_mlton_usertimes.sh $DEST/cpp.$NAME.out`;;
      cpp_df)   runcpp_df;   TIMES+=" "`extract_mlton_usertimes.sh $DEST/cppdf.$NAME.out`;;
      cpp_corefit) runcpp_corefit; 
                             TIMES+=" "`extract_mlton_usertimes.sh $DEST/cppnew.$NAME.out`;;
      cpp_corefit_nothreads) runcpp_corefit_nothreads; 
                             TIMES+=" "`extract_mlton_usertimes.sh $DEST/cppnothreads.$NAME.out`;; 

      c2)        C2OPTIONS="-O3 -gc refcount -sigseg copyalways -nothreads";
                 runc2; 
                 TIMES="$TIMES `extract_mlton_usertimes.sh $DEST/c2.$NAME.out`";;

      c2def)     C2OPTIONS="-O3 -gc deferred -sigseg copyalways -nothreads";
                 runc2 def;       
                 TIMES+=" "`extract_mlton_usertimes.sh $DEST/c2def.$NAME.out`;;

      c2seglist) C2OPTIONS="-O3 -gc refcount -sigseg seglist -nothreads";
                 runc2 seglist;      
                 TIMES+=" "`extract_mlton_usertimes.sh $DEST/c2seglist.$NAME.out`;; 

      c2defseglist) C2OPTIONS="-O3 -gc deferred -sigseg seglist -nothreads";
                    runc2 defseglist;      
                    TIMES+=" "`extract_mlton_usertimes.sh $DEST/c2defseglist.$NAME.out`;; 

      c2boehm)      C2OPTIONS="-O3 -gc boehm -sigseg copyalways -nothreads";
                    runc2 boehm;
                    TIMES+=" "`extract_mlton_usertimes.sh $DEST/c2boehm.$NAME.out`;;

      c2boehmseglist) C2OPTIONS="-O3 -gc boehm -sigseg seglist -nothreads";
                      runc2 boehmseglist;       
                      TIMES+=" "`extract_mlton_usertimes.sh $DEST/c2boehmseglist.$NAME.out`;;


      *) echo Unhandled backend: $backend; exit -1;;
    esac
  done

  echo ALLDONE, times were: $TIMES
  echo $NAME $TIMES >> RESULTS.txt
}


function dump_plot_script() {
    FILE=$1
    RESULTS=$2

    # Terrible way to get the length:
    #len=`echo $BACKENDS | xargs -i echo | wc -l`
    len=0
    for bk in $BACKENDS; do len=$((len+1)); done
    echo;echo Generating plot script for $len backends: $BACKENDS

    cd $START
    cat > $FILE <<EOF
# set title "Hand-optimized marmot application"
load "../shared.gp"
EOF

    PLOTLINE="plot '"$RESULTS"' using 2:xtic(1) title col"

    i=3
    #for bk in $BACKENDS; do
    while [ $i -lt $((len+2)) ]; do
    #echo Backend: $i
	PLOTLINE+=", '' using $i title col"
	i=$((i+1))
    done

    echo $PLOTLINE >> $FILE
    echo ... finished
}
