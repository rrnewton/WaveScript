#!/bin/sh


source ../shared.sh

function go() {
  REP=$1
  RANGE=$2
  PLOT=plot_"$REP".gp
  #FIELDS=`cat $REP.result | wc -l`
  FIELDS=`tail -n 1 $REP.result | wc -w`
  export BACKENDS="`seq 1 $((FIELDS - 1))`"
  echo BACKENDS : $BACKENDS
  echo set title \"Data Representation: $1\"           > $PLOT
  echo "set yrange [ 0.00000 : $2 ] noreverse nowriteback" >> $PLOT
  dump_plot_script $PLOT $REP.result
  gnuplot $PLOT > $1.eps
}

go alloc_arrarr 46000.
go  fold_arrarr 46000.

go alloc_arraytuple 22000.
go  fold_arraytuple 22000.
go alloc_tuplearray 22000.
go  fold_tuplearray 22000.
 
echo "Convert all to pdf."
ls *.eps | xargs -n1 ps2pdf

