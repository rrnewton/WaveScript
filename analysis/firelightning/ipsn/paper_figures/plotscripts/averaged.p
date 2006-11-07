
set xlabel 'Local temperature threshold, centigrade'
set ylabel 'Average lag between fire start and detection at base station.'

plot "../data/averaged_noise1.dat" using 1:4 with linespoints lw 2, \
     "../data/averaged_noise2.dat" using 1:4 with linespoints lw 2, \
     "../data/averaged_noise3.dat" using 1:4 with linespoints lw 2, \
     "../data/averaged_noise4.dat" using 1:4 with linespoints lw 2, \
     "../data/averaged_noise5.dat" using 1:4 with linespoints lw 2, \
     "../data/averaged_noise6.dat" using 1:4 with linespoints lw 2  
#     "../data/averaged_noise1.dat" using 1:4:($5) with errorbars, \
#     "../data/averaged_noise2.dat" using 1:4:($5) with errorbars, \
#     "../data/averaged_noise3.dat" using 1:4:($5) with errorbars, \
#     "../data/averaged_noise4.dat" using 1:4:($5) with errorbars, \
#     "../data/averaged_noise5.dat" using 1:4:($5) with errorbars, \
#     "../data/averaged_noise6.dat" using 1:4:($5) with errorbars
	
#"../data/inlined.dat" using 1:2:($3/10) with errorbars
#"err.dat" using 1:2:3:4 with errorbars

pause -1 "Press enter..."
