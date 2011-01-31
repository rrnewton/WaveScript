# This header is attached to all of the benchmark bargraphs that are
# generated by the scripts in the subdirectories of the benchmarks/
# directory.

#set terminal pdf
set terminal postscript color

set boxwidth 0.9 absolute
set style fill  solid 1.00 border -1
set style histogram clustered gap 1 title  offset character 0, 0, 0
set datafile missing '-'
set style data histograms
#set xtics border in scale 1,0.5 nomirror rotate by -45  offset character 0, 0, 0 
set xtics border in scale 1,0.5 nomirror 

# plot 'RESULTS.txt' using 2:xtic(1) title col, '' using 3 title col, '' using 4 title col

# I don't know how to do autoscale but then make sure there's a little bit of a margin.
set yrange [0:*]
#set autoscale y
#set borders 3
set offsets 0, 0, 0.1, 0