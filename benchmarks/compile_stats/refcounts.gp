
set title "Number of Refcount increments/decrements"

load "../shared.gp"
#set terminal x11 

set style histogram clustered gap 2 title  offset character 0, 0, 0
set xtics border in scale 1,0.5 nomirror rotate by -45  offset character 0, 0, 0 

set ylabel "Percentage of pointers on heap as opposed to stack"
set y2label "RC instructions percentage of non-leaf AST nodes"
#show y2label
set y2tics 

# Just the basic refcount counts:
# plot 'refcounts.log' using 2:xtic(1) title col, '' using 3 title col, '' using 4 title col


plot 'refcounts.log' using ($4/$2):xtic(1) title 'PercentageHeapPtrs' axes x1y1, \
                  '' using ($2/$5):xtic(1) title 'RefcountDensity' axes x1y2
#                  '' using ($5/$2):xtic(1) title 'CodeperRC' axes x1y2

#plot 'refcounts.log' using 2:xtic(1) title col, '' using 3 title col, '' using 4 title col, '' using (10000 * ($4 / $3)):xtic(1) title col

#plot 'refcounts.log' using ($4 / $3):xtic(1) title col

