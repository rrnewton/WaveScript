#!/bin/bash

#prefix=bhatta
#prefix=bhattaInt
#preindex=000

prefix=my_int
preindex=_


echo Diffing diffs...
for ind in `seq 0 19`; 
do
 echo "$ind "
 diff processed/Diff_"$ind".bmp processed/FeederStation*/$prefix*/Diff*"$preindex""$ind".*;
done 

echo
echo ================================================================================
echo Diffing masks...
for ind in `seq 0 19`; 
do
 echo "$ind "
 diff processed/Mask_"$ind".bmp processed/FeederStation*/$prefix*/Mask*"$preindex""$ind".*;
done 

echo
echo ================================================================================
echo Diffing FG images...
for ind in `seq 0 19`; 
do
 echo "$ind "
 diff processed/Fg_"$ind".bmp processed/FeederStation*/$prefix*/Fg*"$preindex""$ind".*;
done 



