
set term postscript eps enhanced monochrome defaultplex "Helvetica" 20

set output "pipeline-speedup.eps"

set title "Pipelining with N worker threads"
set xlabel "Number of Worker CPUs"
set ylabel "Speedup over benchmark timing"
set key top left
set yrange [*:*]
plot [-.5:15.5] "buildsplit_600_600_stats.txt" u 1:2:5 w errorbars t "buildsplit" lt 1, \
      "mapsplit_600_600_stats.txt" u 1:2:5 w errorbars t "mapsplit" lt 2, \
      "fullpar_600_600_stats.txt" u 1:2:5 w errorbars t "fullsplit" lt 3, \
      "buildsplit_600_600_stats.txt" u 1:2 w l t ""  lt 1, \
      "mapsplit_600_600_stats.txt" u 1:2 w l t ""  lt 2, \
      "fullpar_600_600_stats.txt" u 1:2 w l  t "" lt 3

set output

set output "pipeline-speedup-30.eps"

set title "Pipelining with N worker threads, Batch size 30"
set xlabel "Number of Worker CPUs"
set ylabel "Speedup over benchmark timing"
set key top left
set yrange [*:*]
plot [-.5:15.5] "buildsplit_600_30_stats.txt" u 1:2:5 w errorbars t "buildsplit" lt 1, \
      "mapsplit_600_30_stats.txt" u 1:2:5 w errorbars t "mapsplit" lt 2, \
      "fullpar_600_30_stats.txt" u 1:2:5 w errorbars t "fullsplit" lt 3, \
      "buildsplit_600_30_stats.txt" u 1:2 w l t ""  lt 1, \
      "mapsplit_600_30_stats.txt" u 1:2 w l t ""  lt 2, \
      "fullpar_600_30_stats.txt" u 1:2 w l  t "" lt 3

set output

set output "nopipeline.eps"

set title "Average Latency with N worker threads"
set xlabel "Number of Worker CPUs"
set ylabel "Average Latency in ms"
set key bottom right
set yrange [10:60]
plot [-.5:15.5] "buildsplit_600_1_stats.txt" u 1:2:5 w errorbars t "buildsplit" lt 1, \
      "mapsplit_600_1_stats.txt" u 1:2:5 w errorbars t "mapsplit" lt 2, \
      "fullpar_600_1_stats.txt" u 1:2:5 w errorbars t "fullsplit" lt 3, \
      "buildsplit_600_1_stats.txt" u 1:2 w l  t "" lt 1, \
      "mapsplit_600_1_stats.txt" u 1:2 w l  t "" lt 2, \
      "fullpar_600_1_stats.txt" u 1:2 w l  t "" lt 3

set output



   
   

