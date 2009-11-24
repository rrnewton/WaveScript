





set terminal postscript lw 3 30;
set key top left; 
set output "road_mileage.ps"

set xlabel "Hours of data logged"
set ylabel ""

# linewidth 2, pointsize 2, setterminal poststript lw 4 
#set autoscale;

plot "/home/newton/data/FINAL_ALLCARS_SMALLGRID.dstlog" using ($1 / 3600):($2 * 0.006 * 1.61) with lines title "Total km Covered" lw 2, \
     "/home/newton/data/FINAL_ALLCARS_SMALLGRID.dstlog" using ($1 / 3600):($3 * 0.006 * 1.61) with lines title "Unique km Covered" lw 2 ;

