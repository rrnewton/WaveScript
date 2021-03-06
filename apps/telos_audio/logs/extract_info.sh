#!/bin/bash

bunzip2 *.bz2

for ((i=1; i<=6; i++)); do
  #grep ")" cut_$i  | sed 's/[(,)]//g' | awk '{ print $1" "$2" "$3 }' > cut_"$i".dat
  grep ")" cut_$i  | sed 's/[(,\./)]/ /g' | awk '{ print "("$1" "$2" "$3"" $4")" }' > cut_"$i".ss
  grep "#\[" cut_$i  | sed 's/[\#,\.\[]/ /g' | awk '{ print "("$1" "$2" "$3" "$4")" }' >> cut_"$i".ss
done 
