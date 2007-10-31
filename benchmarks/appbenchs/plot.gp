


set title "Hand-optimized marmot application" 

set yrange [ 0.00000 : 22000. ] noreverse nowriteback
load "../shared.gp"
plot 'RESULTS.txt' using 2:xtic(1) title col, '' using 3 title col, '' using 4 title col

# set boxwidth 0.9 absolute
# set style fill  solid 1.00 border -1
# set style histogram clustered gap 2 title  offset character 0, 0, 0
# set datafile missing '-'
# set style data histograms
# #set xtics border in scale 1,0.5 nomirror rotate by -45  offset character 0, 0, 0 
# set xtics border in scale 1,0.5 nomirror 

# set yrange [ 0.00000 : 17000. ] noreverse nowriteback
# plot 'RESULTS.txt' using 2:xtic(1) title col, '' using 3 title col, '' using 4 title col

