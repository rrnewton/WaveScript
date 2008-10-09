#!/bin/bash

for ((i=1; i<=6; i++)); do
  #grep ")" cut_$i  | sed 's/[(,)]//g' | awk '{ print $1" "$2" "$3 }' > cut_"$i".dat
  grep ")" cut_$i  | sed 's/[(,\./)]/ /g' | awk '{ print "("$1" "$2" "$3")" }' > cut_"$i".ss
done 
