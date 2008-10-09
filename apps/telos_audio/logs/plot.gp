

# set y axis

plot 'summary.dat' using 1:2 title col w lp, \
     '' using 1:4 title col w lp, \
     '' using 1:($2 * $4 / 100 ) title 'product' w lp     

     # plot 'RESULTS.txt' using 2:xtic(1) title col, '' using 3 title col, '' using 4 title col
