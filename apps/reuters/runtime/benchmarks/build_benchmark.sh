#!/bin/bash

set -e

first_stage=$1
shift

#  WSQ_CC=icc 
export WSQ_GC=refcount 
export WSQ_OPTLVL=O3 
export WSQ_VERBOSE=1
echo
echo "Compiling query $first_stage with WaveScript:" 
echo "================================================================================" 
./$first_stage $*
# TAQ.1000000
