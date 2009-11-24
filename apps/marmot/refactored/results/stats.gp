
set term postscript eps enhanced monochrome defaultplex "Helvetica" 20

set output "paramap-speedup.eps"

set title "Speedup from Parmapped AML with N worker threads"
set xlabel "Number of Worker CPUs"
set ylabel "Speedup over single CPU"
set nokey
plot  "/tmp/stats" u 1:2:5 w errorbars t "Speedup",  "/tmp/stats" u 1:2 w lines t ""

set output


set title ""
set xlabel "Number of Worker CPUs"
set ylabel "Speedup over single CPU"
set key
plot  "/tmp/grids.8" u 1:2:5 w errorbars t "size 8",  \
  "/tmp/grids.16" u 1:2:5 w errorbars t "size 16",  \
  "/tmp/grids.32" u 1:2:5 w errorbars t "size 32",  \
  "/tmp/grids.64" u 1:2:5 w errorbars t "size 64",  \
  "/tmp/grids.128" u 1:2:5 w errorbars t "size 128",  \
  "/tmp/grids.256" u 1:2:5 w errorbars t "size 256",  \
  "/tmp/grids.512" u 1:2:5 w errorbars t "size 512",  \
  "/tmp/grids.8" u 1:2 w l t "",  \
  "/tmp/grids.16" u 1:2 w l t "",  \
  "/tmp/grids.32" u 1:2 w l t "",  \
  "/tmp/grids.64" u 1:2 w l t "",  \
  "/tmp/grids.128" u 1:2 w l t "",  \
  "/tmp/grids.256" u 1:2 w l t "",  \
  "/tmp/grids.512" u 1:2 w l t "" 

   
   

