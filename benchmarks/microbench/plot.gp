
#set terminal png transparent nocrop enhanced font arial 8 size 420,320 
#set terminal png nocrop enhanced font arial 8 
# set output 'histograms.2.png'

#set terminal png enhanced
#set terminal png font arial 8 
#set terminal png nocrop
#set terminal pdf
set terminal postscript color
#set terminal epslatex
#set terminal cairopdf

set title "Microbenchmarks in all WS backends" 



# set terminal png transparent nocrop enhanced font arial 8 size 420,320 
# set output 'histograms.2.png'
set boxwidth 0.9 absolute
set style fill  solid 1.00 border -1
set style histogram clustered gap 2 title  offset character 0, 0, 0
set datafile missing '-'
set style data histograms
#set xtics border in scale 1,0.5 nomirror rotate by -45  offset character 0, 0, 0 
set xtics border in scale 1,0.5 nomirror 
# offset  character 0, 0, 0 

set yrange [ 0.00000 : 17000. ] noreverse nowriteback
#plot 'immigration.txt' using 6:xtic(1) ti col, '' u 12 ti col, '' u 13 ti col, '' u 14 ti col
#plot 'RESULTS.txt' using 2:xtic(1) ti col, '' u 2 ti col, '' u 3 ti col, '' u 4 ti col

plot 'RESULTS.txt' using 2:xtic(1) title col, '' using 3 title col, '' using 4 title col


#set xtics  ("timer" 0.00000, "file_bigchunks" 1.00000, "file_smallchunks" 2.00000)

