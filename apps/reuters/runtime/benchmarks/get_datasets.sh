#!/bin/bash

# Download the datasets from the web.  These are too bulky to stick in
# the git repository.

function download() {
 file=TAQ.$1
 if [ -e $file ]; then
   echo "File $file already exists."
 else 
#   wget http://wasp.ffh.us/datasets/reuters/$file
   wget http://cs.indiana.edu/~rrnewton/datasets/reuters/$file.gz
   gunzip $file.gz
 fi
}

download 1000
download 10000
download 100000
download 500000
download 1000000
# # This one is a full gigabyte (once unpacked):
# download 5206325
