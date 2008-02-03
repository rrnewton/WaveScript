
set title "Context of Array Allocation Sites"

load "../shared.gp"
#set terminal x11 

set style histogram clustered gap 3 title  offset character 0, 0, 0
set xtics border in scale 1,0.5 nomirror rotate by -45  offset character 0, 0, 0 

#set y2label "RCPerCode"
#show y2label
set y2tics 

plot 'alloc_sites.log' using 5:xtic(1) title 'OnTrunkStaticNoEscape', '' using 2 title 'OnTrunkStatic', '' using 3 title 'OnTrunkDynamic', '' using 4 title 'OffTrunk'



