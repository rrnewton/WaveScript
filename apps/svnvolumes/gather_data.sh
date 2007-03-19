#!/bin/bash

## This script goes through the revisions and counts how many lines
## were added/modified in each checkin.


#OUTFILE=regiment_traffic.txt
#REPOS='https://macroprogramming.ffh.us/repos/'

OUTFILE=wavescope_traffic.txt
REPOS='svn+ssh://newton@nms.csail.mit.edu/export/home2/svn/WaveScope/trunk/code/v1'


if [ -f "$OUTFILE" ];
then STARTREV=`tail $OUTFILE -n1 | awk '{ print $1 }'`;
     STARTREV=$(($STARTREV+1));
else STARTREV=1
fi

# The latest rev is as far as we go:
which svn > /dev/null
svn info -rHEAD $REPOS | grep Revision | sed s/Revision:\ // > svn_rev.txt
ENDREV=`cat svn_rev.txt`;

echo Running from revision $STARTREV to $ENDREV

REV=$STARTREV
#for REV in `seq $STARTREV $ENDREV`; 
until [ "$REV" -gt "$ENDREV" ]
do 
    LINES=`svn diff -r $(($REV-1)):$REV $REPOS | grep "^+" | wc -l` 
    DATE=`svn info -r $REV https://macroprogramming.ffh.us/repos/  | grep Date | awk '{ print $4 }' | sed 's/-/ /g'`
    echo $REV $DATE $LINES >> $OUTFILE
    echo $REV $DATE $LINES
    REV=$(($REV+1))
done
