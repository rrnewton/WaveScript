#!/bin/bash

CPU0=`grep --binary-files=text STARTTIMECPU $1 | awk '{ print $2; print "\n" }'`
CPU1=`grep --binary-files=text ENDTIMECPU $1 | awk '{ print $2; print "\n" }'`


#REAL0=`grep STARTTIMEREAL $1 | awk '{ print $2}'`
#REAL1=`grep ENDTIMEREAL $1 | awk '{ print $2}'`

#echo "times <"$CPU0">  and <"$CPU1">"
#echo "times <"$REAL0">  and <"$REAL1">"

# Don't know how to do this in bash right now:
echo "(begin (map (lambda (ls) (pretty-print (apply - ls))) (reverse (map list (list $CPU1) (list $CPU0)))) (void))" | petite -q

#for STRT in $CPU0; END  in $CPU1;  do
#for STRT, END in $CPU0, $CPU1;  do
#  echo $(($END - $STRT))
#done

#ELAPSED1=$(($CPU1 - $CPU0))
#ELAPSED2=$(($REAL1 - $REAL0))

#echo DIFF $ELAPSED1 foo $ELAPSED2

# Currently just print the CPU time
#echo $ELAPSED1
