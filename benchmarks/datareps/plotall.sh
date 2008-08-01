#!/bin/sh


source ../shared.sh

function go3() {
  REP=$1
  RANGE=$2
  PLOT=plot_"$REP".gp
  echo set title \"Data Representation: $1\"           > $PLOT
  set "yrange [ 0.00000 : $2 ] noreverse nowriteback" >> $PLOT
  dump_plot_script $PLOT $REP.result
  gnuplot $PLOT > $1.eps
}

# function go3() {
#  echo "Plotting..."
#   echo 'set title "Data Representation: '$1'"
#   set yrange [ 0.00000 : '$2' ] noreverse nowriteback 
#   load "../shared.gp"
#   plot "'$1'.result" using 2:xtic(1) title col, "" using 3 title col, "" using 4 title col; ' \
#  | gnuplot > $1.eps
# }

# function go2() {
#  echo "Plotting..."
# echo 'set title "Data Representation: '$1'";
#       set yrange [ 0.00000 : '$2' ] noreverse nowriteback;
#       load "../shared.gp";
#       plot "'$1'.result" using 2:xtic(1) title col, "" using 3 title col; ' \
#     | gnuplot > $1.eps
# }

go3 alloc_arrarr 46000.

exit 

go3  fold_arrarr 46000.

go2 alloc_arraytuple 22000.
go2  fold_arraytuple 22000.
go2 alloc_tuplearray 22000.
go2  fold_tuplearray 22000.

echo "Convert all to pdf."
ls *.eps | xargs -n1 ps2pdf

