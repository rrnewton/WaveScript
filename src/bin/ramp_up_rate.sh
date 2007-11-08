#!/bin/bash

OUTPUT=$1
START=$2
INTERVAL=$3
END=$4

shift;
shift;
shift;
shift;

QUERY=$*

echo "OUTPUT LOCATION <$OUTPUT>"

echo "## Rate and Realtime " > RESULTS.txt

for rate in `seq $START $INTERVAL $END`; do
  export TIMEROVERIDE=$rate
  FILE=$OUTPUT"_$rate".txt
  RETURN=1
  while [ "$RETURN" != 0 ]; do 
   echo Executing command "$QUERY &> $FILE"  
   $QUERY $> $FILE
   RETURN=$?
   if [ "$RETURN" != 0 ]; then echo "  Retrying..."; fi   
#   echo RETURN WAS $RETURN
#   RETURN=0
  done
#  extract_startend_times.sh $FILE
  echo $rate `extract_startend_real.sh $FILE` >> RESULTS.txt
done

