#!/bin/bash

set -e 

file="$1"
output=`wc -l $file`

set -- $output

echo $1
