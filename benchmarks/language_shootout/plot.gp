


load "../shared.gp"

#set yrange [ 0.00000 : 200000. ] noreverse nowriteback

#plot 'RESULTS.txt' using 2:xtic(1) title col, '' using 3 title col, '' using 4 title col, '' using 5 title col, '' using 6 title col, '' using 7 title col

load "../standard_plot.gp"
