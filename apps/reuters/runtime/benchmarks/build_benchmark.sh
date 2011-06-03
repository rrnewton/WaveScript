#!/bin/bash

set -e

first_stage=$1

#  WSQ_CC=icc 
export WSQ_GC=refcount 
export WSQ_OPTLVL=3 
export WSQ_VERBOSE=1
echo
echo "Compiling query $first_stage with WaveScript:" 
echo "================================================================================" 
./$first_stage

