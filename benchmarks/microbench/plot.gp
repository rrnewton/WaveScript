# set title "Hand-optimized marmot application"
load "../shared.gp"
plot 'RESULTS_misc.txt' using 2:xtic(1) title col, '' using 3 title col, '' using 4 title col, '' using 5 title col, '' using 6 title col
