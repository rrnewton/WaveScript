
## This is the list of backends to test, valid symbols are:

#    scheme, schemeO3
#    mlton, mltonO3 
#    cpp, cpp_df, cpp_corefit, cpp_corefit_nothreads -- The old XStream/C++ backend
#    c2 -- the new C backend

#BACKENDS="scheme mlton mltonO3"
if [ "$BACKENDS" = "" ]; 
then BACKENDS="mltonO3 c2boehm c2boehmseglist c2 c2seglist c2def c2defseglist"
fi

WSOPTIONS+=" -exit-error"

OLDWSCARGS="-j 1 --at_once"
WSCARGS="-j 1"

OUTPUTFILE=RESULTS.txt

function print_results_header() {
  if [ "$1" = "" ];
  then local OUT=$OUTPUTFILE
  else local OUT=$1
  fi 
  echo "## Real or User time for each benchmark/backend " > $OUT
  echo "## LD_PRELOAD: $LD_PRELOAD" >> $OUT
  echo "## NOSUDO: $NOSUDO" >> $OUT
  echo "## NICE: $NICE" >> $OUT
  echo Benchmark $BACKENDS >> $OUT
}

function runscheme() {
  echo "  scheme: running... -n $TUPS"
  if ! (ws $FILE $WSOPTIONS -n $TUPS -t &> $DEST/scheme.$NAME"_"$LIMITCPUS.out); 
  then echo "ws failed!"; exit -1; fi
}

function runschemeO3() {
  echo "  scheme -O3: running... -n $TUPS"
  if ws.opt $FILE -O3 $WSOPTIONS -n $TUPS -t &> $DEST/schemeO3.$NAME"_"$LIMITCPUS.out; then echo>/dev/null;
  else echo "ws.opt failed!"; exit -1; fi
}

function runcpp() {
  export WAVESCOPED="$REGIMENTD/benchmarks/engine/1495"

  echo "  cpp: compiling..."
  rm -f "$WAVESCOPED/libws-SMSegList.a" 
  ln -s "$REGIMENTD/benchmarks/libws-SMSegList.1495.O2.default.a" "$WAVESCOPED/libws-SMSegList.a" 
  if wsc $FILE -t $WSOPTIONS   &> $DEST/cpp.compile.$NAME"_"$LIMITCPUS.out; then echo>/dev/null;
  else echo "wsc failed!"; exit -1; fi
  echo "    cpp: running... -n $TUPS $OLDWSCARGS"
  if ! (time ./query.exe $OLDWSCARGS -n $TUPS) &> $DEST/cpp.$NAME"_"$LIMITCPUS.out; 
  then echo "failed!"; exit -1; fi
  rm -f query.*  
}

function runcpp_df() {
  export WAVESCOPED="$REGIMENTD/benchmarks/engine/1495"
  echo "  cpp: -DDEPTH_FIRST compiling..."
  rm -f "$WAVESCOPED/libws-SMSegList.a" 
  ln -s "$REGIMENTD/benchmarks/libws-SMSegList.1495.O2.df.a" "$WAVESCOPED/libws-SMSegList.a" 
  if  wsc $FILE -t $WSOPTIONS --scheduler depth-first &> $DEST/cppdf.compile.$NAME"_"$LIMITCPUS.out; then echo>/dev/null;
  else echo "wsc failed!"; exit -1; fi
  echo "    cpp: running... -n $TUPS $OLDWSCARGS"
  if ! (time ./query.exe $OLDWSCARGS -n $TUPS) &> $DEST/cppdf.$NAME"_"$LIMITCPUS.out;
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
  if  wsc $FILE -t --scheduler corefit-scheduler-df $WSOPTIONS  &> $DEST/cppnew.compile.$NAME"_"$LIMITCPUS.out; then echo>/dev/null;
  else echo "wsc failed!"; exit -1; fi
  echo "    cpp: running... -n $TUPS $WSCARGS"
  if ! (time ./query.exe $WSCARGS -n $TUPS) &> $DEST/cppnew.$NAME"_"$LIMITCPUS.out;
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
  if  wsc $FILE -t -nothreads --scheduler corefit-scheduler-df $WSOPTIONS  &> $DEST/cppnothreads.compile.$NAME"_"$LIMITCPUS.out; then echo>/dev/null;
  else echo "wsc failed!"; exit -1; fi
  echo "    cppnothreads: running... -n $TUPS $WSCARGS"
  if ! (time ./query.exe $WSCARGS -n $TUPS) &> $DEST/cppnothreads.$NAME"_"$LIMITCPUS.out;
  then echo "failed!"; exit -1; fi
  rm -f query.*  
  unset COREFITBENCH
}

# Mlton and caml are run almost identically:
CAMLOPTIONS=
function runcaml() {
  echo "  caml $MLTONOPTIONS: compiling..."
  if wscaml $FILE $MLTONOPTIONS $WSOPTIONS &> $DEST/caml$1.compile.$NAME"_"$LIMITCPUS.out; then echo>/dev/null;
  else echo "wscaml failed!"; exit -1; fi
  echo "   caml: running... -n "$TUPS
  if ! (time ./query.caml.exe -n $TUPS) &> $DEST/caml$1.$NAME"_"$LIMITCPUS.out; 
  then echo "failed!"; exit -1; fi
}

MLTONOPTIONS=
function runmlton() {
  echo "  mlton $MLTONOPTIONS: compiling..."
  if wsmlton $FILE $MLTONOPTIONS $WSOPTIONS &> $DEST/mlton$1.compile.$NAME"_"$LIMITCPUS.out; then echo>/dev/null;
  else echo "wsmlton failed!"; exit -1; fi
  echo "   mlton: running... -n "$TUPS
  if ! (time ./query.mlton.exe -n $TUPS) &> $DEST/mlton$1.$NAME"_"$LIMITCPUS.out; 
  then echo "failed!"; exit -1; fi
}

#C2OPTIONS=
function runc2() {
  echo "  wsc2 $C2OPTIONS : compiling..."
  echo RUNNING COMMAND: wsc2 $FILE $C2OPTIONS $WSOPTIONS
  if wsc2 $FILE $C2OPTIONS $WSOPTIONS &> $DEST/c2$1.compile.$NAME"_"$LIMITCPUS.out; then echo>/dev/null;
  else echo "wsc2 failed!"; exit -1; fi
  echo "   wsc2: running output... -n "$TUPS

  if [ "$LIMITCPUS" = "" ] || [ "$LIMITCPUS" = "0" ]; then
    if ! (time ./query.exe -n $TUPS) &> $DEST/c2$1.$NAME"_"$LIMITCPUS.out; 
    then echo "failed!"; exit -1; fi
  else
    if ! (run_with_n_cpus $LIMITCPUS ./query.exe -n $TUPS) &> $DEST/c2$1.$NAME"_"$LIMITCPUS.out; 
    then echo "failed!"; exit -1; fi
  fi
}



function add_time() {
  TIMES+=" "`extract_$1_$WHICHTIME.sh $2`;
}

## This is where you disable/enable backends.
## Arguments: filename sans extension, output file, in tuple limit, out tuple limit
## Currently in tuple limit is unimplemented.
function runallbackends() {
  C2OPTIONSBAK=$C2OPTIONS
  NAME=$1
  FILE=$1.ws
  DEST=$2
  INTUPS=$3
  TUPS=$4
  echo;echo "  $FILE -- Running all backends: INTUPS: $INTUPS  OUTTUPS: $TUPS";

  # Clean up:
  rm -rf query.*  
  echo "Invoking the following backends: $BACKENDS"
  
  if [ "$WHICHTIME" = "" ];     then WHICHTIME=usertimes; fi
  if [ "$WHICHTIME" = "user" ]; then WHICHTIME=usertimes; fi
  if [ "$WHICHTIME" = "real" ]; then WHICHTIME=realtimes; fi  

  if [ "$C2OPTLVL" = "" ];      then C2OPTLVL="-O3"; fi

  TIMES=
  for backend in $BACKENDS; do
    case $backend in
      scheme)   runscheme;   add_time scheme $DEST/scheme.$NAME"_"$LIMITCPUS.out;;
      schemeO3) runschemeO3; add_time scheme $DEST/schemeO3.$NAME"_"$LIMITCPUS.out;;

      camlO3)   MLTONOPTIONS="-O3"; 
                runcaml O3;
                add_time mlton $DEST/camlO3.$NAME"_"$LIMITCPUS.out;;

      mlton)    runmlton;    add_time mlton $DEST/mlton.$NAME"_"$LIMITCPUS.out;;
      mltonO3)  MLTONOPTIONS="-O3"; 
                runmlton O3; add_time mlton $DEST/mltonO3.$NAME"_"$LIMITCPUS.out;;
      cpp)      runcpp;      add_time mlton $DEST/cpp.$NAME"_"$LIMITCPUS.out;;
      cpp_df)   runcpp_df;   add_time mlton $DEST/cppdf.$NAME"_"$LIMITCPUS.out;;
      cpp_corefit) runcpp_corefit; 
                             add_time mlton $DEST/cppnew.$NAME"_"$LIMITCPUS.out;;
      cpp_corefit_nothreads) runcpp_corefit_nothreads; 
                             add_time mlton $DEST/cppnothreads.$NAME"_"$LIMITCPUS.out;;

      c2)        C2OPTIONS+="$C2OPTLVL -gc refcount -sigseg copyalways ";
                 runc2; 
                 add_time shell $DEST/c2.$NAME"_"$LIMITCPUS.out;;

      c2def)     C2OPTIONS+="$C2OPTLVL -gc deferred -sigseg copyalways ";
                 runc2 def;       
                 add_time shell $DEST/c2def.$NAME"_"$LIMITCPUS.out;;

      c2seglist) C2OPTIONS+="$C2OPTLVL -gc refcount -sigseg seglist ";
                 runc2 seglist;      
                 add_time shell $DEST/c2seglist.$NAME"_"$LIMITCPUS.out;; 

      c2defseglist) C2OPTIONS+="$C2OPTLVL -gc deferred -sigseg seglist ";
                    runc2 defseglist;      
                    add_time shell $DEST/c2defseglist.$NAME"_"$LIMITCPUS.out;; 

      c2boehm)      C2OPTIONS+="$C2OPTLVL -gc boehm -sigseg copyalways ";
                    runc2 boehm;
                    add_time shell $DEST/c2boehm.$NAME"_"$LIMITCPUS.out;;

      c2boehmseglist) C2OPTIONS+="$C2OPTLVL -gc boehm -sigseg seglist ";
                      runc2 boehmseglist;       
                      add_time shell $DEST/c2boehmseglist.$NAME"_"$LIMITCPUS.out;;


      *) echo Unhandled backend: $backend; exit -1;;
    esac
    # Restore the options each time.
    C2OPTIONS=$C2OPTIONSBAK
  done

  echo ALLDONE, times were: $TIMES
  echo $NAME $LIMITCPUS $TIMES >> $OUTPUTFILE
}


function dump_plot_script() {
    local FILE=$1
    local RESULTS=$2
    local NORM=$RESULTS".normalized"

    # I started to need a real program here.  Call the scheme script.
    export BACKENDS
    #$REGIMENTD/backends/dump_plot_script.ss $FILE $RESULTS 

    if [ "$NORMALIZE" != "" ];
    then 
      echo "Normalizing timings, sending output to new file: $NORM"
      if ! $REGIMENTD/benchmarks/normalize_timings.ss $RESULTS $NORM; then exit 1; fi
    else 
      NORM=$RESULTS
    fi

    # Terrible way to get the length:
    #len=`echo $BACKENDS | xargs -i echo | wc -l`
    len=0
    for bk in $BACKENDS; do len=$((len+1)); done
    echo;echo Generating plot script for $len backends: $BACKENDS

    cat >> $FILE <<EOF
load "../shared.gp"
EOF
    PLOTLINE="plot '"$NORM"' using 2:xtic(1) title col"
    i=3
    while [ $i -lt $((len+2)) ]; do
	PLOTLINE+=", '' using $i title col"
	i=$((i+1))
    done
    echo $PLOTLINE >> $FILE
    echo ... finished
}



function run_multithreaded() {
  NAME=$1
  TEMPDIR=$2
  TUPS=$3

  # Running with LIMITCPUS=0 is a shorthand for no threads:
  echo;echo "RUNNING $NAME WITH THREADS DISABLED."
  export LIMITCPUS=0;
  export C2OPTIONS=" "
  runallbackends $NAME  $TEMPDIR __ $TUPS

  for ((lim = 1; lim <= $NUMCPUS; lim++)) do
    export C2OPTIONS=" -threads "
    export LIMITCPUS=$lim;
    # For this benchmark, we only split it as many ways as we have CPUs:
    export WORKERS=$lim;
    echo;echo "RUNNING $NAME WITH $LIMITCPUS CPU(S)."
    runallbackends $NAME  $TEMPDIR __ $TUPS
  done
  unset C2OPTLVL
}
