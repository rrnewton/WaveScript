#!/bin/bash


# This will rank the quality of a solution according to the objective function specified.

SOLVER=lp_solve

TEMP=`tempfile`

OPT=$1
PROBLEM=$2
ANSWER=$3

if [ "$3" == "" ]; then
 echo "Usage: ./rank_solution.ss <optimizationObjective> <origProblemFile> <answers>"
 exit 1
fi

cat $PROBLEM $ANSWER > $TEMP

# echo "Filled up temp file $TEMP"

#FORMULATE=./formulate_ilp.ss
#FORMULATE= formulate_ilp.ss

FORMULATE=`dirname $0`/formulate_ilp.ss
#TEMP2=`tempfile`

#echo RUNNING FROM `dirname $0`
#echo FORMMULATE $FORMULATE , temp2 $TEMP2

#$FORMULATE $OPT $TEMP > $TEMP2
#lp_solve $TEMP2 | grep Value

TEMP2=$ANSWER".dump"
$FORMULATE $OPT $TEMP | $SOLVER > $TEMP2
grep "Value of objective" $TEMP2

#  $FORMULATE $OPT $TEMP | lp_solve | grep -v " 0"
