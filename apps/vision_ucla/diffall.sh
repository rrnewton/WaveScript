#!/bin/bash

#prefix=bhatta
#prefix=bhattaInt
#preindex=000

prefix=my_int
preindex=_

SEQ="0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19"

echo Diffing diffs...
for ind in $SEQ; 
do
 echo "$ind "
 diff processed/Diff_"$ind".bmp processed/FeederStation*/$prefix*/Diff*"$preindex""$ind".*;
done 

echo
echo ================================================================================
echo Diffing masks...
for ind in $SEQ;
do
 echo "$ind "
 diff processed/Mask_"$ind".bmp processed/FeederStation*/$prefix*/Mask*"$preindex""$ind".*;
done 

echo
echo ================================================================================
echo Diffing FG images...
for ind in $SEQ;
do
 echo "$ind "
 diff processed/Fg_"$ind".bmp processed/FeederStation*/$prefix*/Fg*"$preindex""$ind".*;
done 



