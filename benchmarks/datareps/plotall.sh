#!/bin/sh

echo "Plotting alloc results."
cp alloc_arrarr.result RESULTS.txt
echo 'set title "Data Representation: Allocating Arrays of Arrays"
 set yrange [ 0.00000 : 46000. ] noreverse nowriteback 
 load "../shared.gp"' | gnuplot > alloc_arrarr.eps

echo "Plotting fold results."
cp fold_arrarr.result RESULTS.txt
echo 'set title "Data Representation: Folding Arrays of Arrays"
 set yrange [ 0.00000 : 46000. ] noreverse nowriteback 
 load "../shared.gp"' | gnuplot > fold_arrarr.eps

echo "Convert all to pdf."
ls *.eps | xargs -n1 ps2pdf

