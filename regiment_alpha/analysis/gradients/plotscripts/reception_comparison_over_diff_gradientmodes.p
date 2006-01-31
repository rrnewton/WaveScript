#set autoscale;

#set multiplot

#set terminal x11 enhanced lw 2 "Times-Roman" 20;

set xlabel 'Average link quality'
set ylabel 'Average number of responses received per epoch'

set label 'Foo Bar Baz' at graph 0.05,0.5 left

plot \
     "../data/inlined.dat" using 1:2 with linespoints lw 2 , \
     "../data/linked.dat" using 1:2 with linespoints lw 2, \
     "../data/etx.dat" using 1:2 with linespoints lw 2, \
     "../data/etx_noretry.dat" using 1:2 with linespoints lw 2, \
     "../data/etx_noretry.dat" using 1:2:($3/10) with errorbars, \
     "../data/etx.dat" using 1:2:($3/10) with errorbars, \
     "../data/linked.dat" using 1:2:($3/10) with errorbars, \
     "../data/inlined.dat" using 1:2:($3/10) with errorbars


#plot "../data/etx.dat" with linespoints;
#plot "../data/etx.dat" with errorbars;

#"err.dat" using 1:2:3:4 with errorbars

#unset multiplot

pause -1 "Press enter..."
