#!/bin/bash


TESTS=" primes mandel sched_tree threadring "
#TESTS=" threadring "

#tries=10
tries=3
cnt=0

#export GHC=6.10
#export GHC=6.11

RESULTS=results.dat

if [ -e $RESULTS ];
then mv $RESULTS "$RESULTS".bak.`date +%s`
fi

echo "# Test variant scheduler threads HASH BestTime" > $RESULTS
echo "# "`date` >> $RESULTS

# Dynamic scoping.  Lame.
function runit() 
{
# for threads in 0 1 2 3 4 5 6 7 8; do
# for threads in 0 4 ; do
  cnt=$((cnt+1))
  echo "----------------------------------------------------------------------"
  echo "  Running Config $cnt: $test $CNC_VARIANT sched $CNC_SCHEDULER threads $threads $hashtab"
  echo "----------------------------------------------------------------------"
  if [ "$threads" != "0" ]
  then export FLAGS=" -threaded "
       export RTS=" +RTS -N$threads "
       echo RTS set $RTS
  else unset RTS
       unset FLAGS
  fi
  if [ "$hashtab" == "" ];
  then HASH="0"
  else HASH="1"
  fi

  # First run it without thread support.
  NORUN=1 ./run $hashtab "$test".hs
  besttime=`./ntimes $tries ./$test $RTS`
  CODE=$?
  if [ "$CODE" != "0" ];
  then echo "ERROR: run_all_tests this test failed completely: $test"
       echo "       Error code $CODE Params: $CNC_VARIANT $CNC_SCHEDULER $FLAGS"
       echo "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
       #exit $CODE
    echo "$test" "$CNC_VARIANT" "$CNC_SCHEDULER" "$threads" "$HASH" "ERR" >> $RESULTS
  else
    echo " >>> BEST TIME $besttime"
    #FILE=$test".dat"
    #echo Output to file $FILE
    echo "$test" "$CNC_VARIANT" "$CNC_SCHEDULER" "$threads" "$HASH" "$besttime" >> $RESULTS
  fi
# done
}



for test in $TESTS; do
  echo "================================================================================"
  echo "                           Running Test: $1                        "
  echo "================================================================================"

#  export CNC_VARIANT=pure
#  export threads=0
#  for i in 1 2; do
#    unset hashtab
#    export CNC_SCHEDULER=$i
#    runit
#  done

 export CNC_VARIANT=io
 for i in 5 6 3; do
   export CNC_SCHEDULER=$i
   #for threads in 4; do
   for threads in 0 1 2 3 4 5 8; do
     export hashtab=""
     runit
     export hashtab="-DHASHTABLE_TEST"
     runit
   done
 done
done

echo "Finished with all test configurations."
