#!/bin/sh





function go3() {
 echo "Plotting..."
  echo 'set title "Data Representation: '$1'"
  set yrange [ 0.00000 : '$2' ] noreverse nowriteback 
  load "../shared.gp"
  plot "'$1'.result" using 2:xtic(1) title col, "" using 3 title col, "" using 4 title col; ' \
 | gnuplot > $1.eps
}

function go2() {
 echo "Plotting..."
echo 'set title "Data Representation: '$1'";
      set yrange [ 0.00000 : '$2' ] noreverse nowriteback;
      load "../shared.gp";
      plot "'$1'.result" using 2:xtic(1) title col, "" using 3 title col; ' \
    | gnuplot > $1.eps
}

go3 alloc_arrarr 46000.
go3  fold_arrarr 46000.

go2 alloc_arraytuple 22000.
go2  fold_arraytuple 22000.
go2 alloc_tuplearray 22000.
go2  fold_tuplearray 22000.

echo "Convert all to pdf."
ls *.eps | xargs -n1 ps2pdf

