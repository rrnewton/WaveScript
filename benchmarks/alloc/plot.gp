
#set terminal pdf
#set terminal postscript color

set logscale x


plot 'mlton.dat' using 1:2 w lp, \
     'plain.dat' using 1:2 w lp , \
     'hoard.dat' using 1:2 w lp , \
     'tcmalloc.dat' using 1:2 w lp 

#     'nedmalloc.dat' using 1:2 w lp 
