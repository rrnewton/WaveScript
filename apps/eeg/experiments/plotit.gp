

#set terminal pdf
set terminal postscript color

set datafile missing '-'

#set xtics border in scale 1,0.5 nomirror rotate by -45  offset character 0, 0, 0 
#set xtics border in scale 1,0.5 nomirror 

set xlabel "Percent CPU used on Tmote Sky"
set ylabel "Number of operators in node partition"
set y2label "Bandwidth of cut"

set y2tics

plot 'results.dat' using ($1)/10:3 title col w lp lw 2, '' using ($1)/10:2 title col axes x1y2 w lp lw 2

