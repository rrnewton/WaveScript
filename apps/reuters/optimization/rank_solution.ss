#!/bin/bash


# This will rank the quality of a solution according to the objective function specified.

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

./formulate_ilp.ss $OPT $TEMP | lp_solve | grep Value



