
set title "Microbenchmarks in all WS backends" 

#set yrange [ 0.00000 : 10000. ] noreverse nowriteback
load "../shared.gp"
set style histogram clustered gap 2 title  offset character 0, 0, 0
set xtics border in scale 1,0.5 nomirror rotate by -45  offset character 0, 0, 0 

#plot 'RESULTS.txt' using 2:xtic(1) title col, '' using 3 title col, '' using 4 title col
plot 'RESULTS.txt' using 2:xtic(1) title col, '' using 3 title col, '' using 4 title col, '' using 5 title col, '' using 6 title col, '' using 7 title col

