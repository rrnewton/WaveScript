#!/bin/bash

function download() {
 file=TAQ.$1
 if [ -e $file ]; then
   echo "File $file already exists."
 else 
   wget http://wasp.ffh.us/datasets/reuters/$file
 fi
}

download 1000
download 10000
download 100000
download 500000
download 1000000
# # This one is a full gigabyte:
# download 5206325
