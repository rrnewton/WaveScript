#!/bin/bash

# This script takes as input a .lp file solvable by lp_solve.  It runs
# lp_solve repeatedly, varying cpu maximum, taking note of how many
# operators were assigned to the node at each point.

# NOTE: the .lp file must have the symbol MAGIC in place of the CPU
# max.  This allows the script to vary the cpu cap.

# Here are the bounds for varying CPU:
# Units are in tenths of a percent:
START=100
INCREMENT=25
STOP=20000

FILE="$1"
RESULTS=results.dat

THREADS=`cat /proc/cpuinfo | grep processor | wc -l`

echo "# Ran on "`date`" on machine "`uname -a` > $RESULTS
echo " cpu bandwidth nodeops " >> $RESULTS


if ! [ -f "$FILE" ];
then echo "Usage: search_cpu_space.sh <file.lp>";
     exit -1;
fi

# Block until there are less than N chidlen of the current shell process.
function block_n_children() {
  N=$1  
}


for i in `seq $START $INCREMENT $STOP`; do
  echo Solving for: $i 
  NEWFILE="$FILE".$i
  cat $FILE | sed "s/MAGIC/$i/" > $NEWFILE
  SOLUTION="$FILE".solve.$i
  lp_solve $NEWFILE > $SOLUTION

  #find /home/ > /dev/null &
  #echo "                   children: " `ps axo ppid | grep $$ | wc -l` 

  BW=`grep "Value of objective" $SOLUTION | awk '{ print $5 }'`
  NODE=`cat $SOLUTION | awk '{ print $2 }' | grep "^1$" | wc -l`
  echo $i $BW $NODE >> $RESULTS
done
