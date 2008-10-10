

set terminal postscript enhanced color

# set y axis

plot 'summary.dat' using 1:2 title 'Percent input received' w lp, \
     '' using 1:4 title 'Percent network msgs received' w lp, \
     '' using 1:($2 * $4 / 100 ) title 'Total throughput (product)' w lp     

#     '' using 1:3 title col w lp, \

     # plot 'RESULTS.txt' using 2:xtic(1) title col, '' using 3 title col, '' using 4 title col
