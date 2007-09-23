#!/bin/sh


for threads in  2 3 4 #`seq 1 8` 
do 
export NUMTHREADS=$threads
#echo | chez_threaded standalone.ss | egrep "real|alloc"
#echo | chez_threaded standalone_pcall.ss | egrep "real|alloc"
echo | chez_threaded standalone_pcall.ss 
done
