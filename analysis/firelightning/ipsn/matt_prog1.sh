#!/bin/sh

#NOISE=0
NOISE=6

for i in `seq 20 50`; do 
#    ./mattlogs.ss -l "./logs/prog1/deadsimple_thresh"$i"_noise"$NOISE".log.gz" \
#    ./mattlogs.ss -l "./logs/250_0noise_thresh0_to_50/deadsimple_thresh"$i"_noise"$NOISE".log.gz" \
#    ./mattlogs.ss -l "./logs/prog2/deadsimple_thresh"$i"_noise"$NOISE".log.gz" \
    ./mattlogs.ss -l "./logs/prog0.log.gz" \
	-o "simple_logs/p0.txt"
done
