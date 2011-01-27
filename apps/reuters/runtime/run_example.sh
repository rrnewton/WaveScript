#!/bin/bash

# This illustrates a conservative method of running the query.

killall query.exe
./example_main.exe

# if [ "$?" != 0 ]; then

echo 
echo "Did any query.exe process outlive its welcome?: "
ps aux | grep query.exe
killall query.exe

echo 
echo "Total lines of output: "
wc -l query_output.log 

echo "You might want to run 'tail -f' on your query output log."
