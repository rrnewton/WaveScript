#!/bin/bash

# This script takes as input a .lp file solvable by lp_solve.  It runs
# lp_solve repeatedly, varying cpu maximum, taking note of how many
# operators were assigned to the node at each point.

# NOTE: the .lp file must have the symbol MAGIC in place of the CPU
# max.  This allows the script to vary the cpu cap.

# Modified this script to spawn multiple threads.  Trying 1 per core
# or 2 per core...

# Here are the bounds for varying CPU:
# Units are in tenths of a percent:
START=6000
INCREMENT=100
STOP=20000

FILE="$1"
RESULTS=results.dat

THREADS=`cat /proc/cpuinfo | grep processor | wc -l`
#THREADS=$(($THREADS*2))
THREADS=$(($THREADS+1))

echo Running with $THREADS threads...

echo "# Ran on "`date`" on machine "`uname -a` > $RESULTS
echo " cpu bandwidth nodeops " >> $RESULTS


if ! [ -f "$FILE" ];
then echo "Usage: search_cpu_space.sh <file.lp>";
     exit -1;
fi

# Block until there are less than N chidlen of the current shell process.
function block_n_children() {
  N=$1  
  CHILDREN=`ps axo ppid | grep $$ | wc -l`

  ## For some reason we ALWAYS have at least one child process.
  #if ! test $CHILDREN -lt $N ;
  if test $CHILDREN -gt $N ;
  then #echo "$CHILDREN" more than "$N" boo;
    sleep 2;
    block_n_children $N
  #else echo woot is "$CHILDREN" less than "$N" ;
  fi
}


for i in `seq $START $INCREMENT $STOP`; do
  echo Solving for: $i 
  NEWFILE="$FILE".$i
  SOLUTION="$FILE".solve.$i
  cat $FILE | sed "s/MAGIC/$i/" > $NEWFILE

  block_n_children $THREADS

  time lp_solve $NEWFILE > $SOLUTION &

  #find /home/ > /dev/null &
  #echo "                   children: " `ps axo ppid | grep $$ | wc -l` 

done

echo Blocking until all children are finished.
block_n_children 1

for i in `seq $START $INCREMENT $STOP`; do
  SOLUTION="$FILE".solve.$i
  BW=`grep "Value of objective" $SOLUTION | awk '{ print $5 }'`
  NODE=`cat $SOLUTION | awk '{ print $2 }' | grep "^1$" | wc -l`
  echo $i $BW $NODE >> $RESULTS
done
