
set xlabel 'Local temperature threshold, centigrade'
set ylabel 'Lag between fire start and detection at base station.'

plot "../data/aggregated_noise1.dat" using ($1 + 0.0):4 with points lw 6, \
     "../data/aggregated_noise2.dat" using ($1 + 0.1):4 with points lw 6, \
     "../data/aggregated_noise3.dat" using ($1 + 0.2):4 with points lw 6, \
     "../data/aggregated_noise4.dat" using ($1 + 0.3):4 with points lw 6, \
     "../data/aggregated_noise5.dat" using ($1 + 0.4):4 with points lw 6, \
     "../data/aggregated_noise6.dat" using ($1 + 0.5):4 with points lw 6 
		  
#"../data/inlined.dat" using 1:2:($3/10) with errorbars
#"err.dat" using 1:2:3:4 with errorbars

pause -1 "Press enter..."
