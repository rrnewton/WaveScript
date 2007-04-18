





set terminal postscript lw 3 30;
#set key top left; 
set output "road_mileage_cdf.ps"

set xlabel "Number of repeat encounters"
set ylabel "Fraction of road segments"

# linewidth 2, pointsize 2, setterminal poststript lw 4 
#set autoscale;

set log xy

#plot "/home/newton/data/FINAL_histogram.dat" using ($1):($2) with points title ""  ps 2  ;
plot "/home/newton/data/FINAL_cdf.dat" using ($1):(($2) / 258021) with linespoints title ""  lw 2 ps 1 ;
