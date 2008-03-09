#!/bin/bash

# This script is called with *one* of the ast_ script's .dat files as input.
# It will make a directory, "temp" and split out the different parts, then graph them.
# If I were better with gnuplot, I might not need to split them into different files.

FILE=$1

rm -rf temp
mkdir temp

grep list-iu-match $FILE   > temp/list-iu-match.dat
grep list-rn-match $FILE   > temp/list-rn-match.dat
grep list-cond $FILE       > temp/list-cond.dat
grep vector-iu-match $FILE > temp/vector-iu-match.dat
grep vector-rn-match $FILE > temp/vector-rn-match.dat
grep vector-cond $FILE     > temp/vector-cond.dat
grep record-predicates $FILE > temp/record-predicates.dat
grep constant-dispatch $FILE > temp/constant-dispatch.dat
grep log-dispatch      $FILE > temp/log-dispatch.dat

