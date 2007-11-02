
set title "Data Representation: Allocating Arrays of Arrays" 
#set yrange [ 0.00000 : 46000. ] noreverse nowriteback
load "../shared.gp"
#set style histogram clustered gap 1 title offset character 0, 0, 0
#plot 'RESULTS.txt' using 2:xtic(1) title col, '' using 3 title col, '' using 4 title col
plot 'alloc_arrarr.result' using 2:xtic(1) title col, '' using 3 title col, '' using 4 title col
