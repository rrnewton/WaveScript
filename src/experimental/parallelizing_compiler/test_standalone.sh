#!/bin/sh


for threads in `seq 1 8` 
do 
export NUMTHREADS=$threads
echo | chez_threaded standalone.ss | egrep "real|alloc"
#echo chez_threaded standalone.ss 
done
