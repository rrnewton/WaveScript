
# [2008.11.06]
# This stress tests the FIFO implementations.

# First, let's test a multi-writer single-reader FIFO, aka "merge":

rm -rf logs; mkdir logs
wsc2 -O3 -threads mwsr.ws -dot

for ((i=0; i<10; i++)); do
  (time ./query.exe -n 10000 > /dev/null) 2> logs/run_$i
done


rm -rf logs; mkdir logs
wsc2 -O3 -threads swsr_and_mwsr.ws -dot

for ((i=0; i<10; i++)); do
  (time ./query.exe -n 10000 > /dev/null) 2> logs/run_$i
done


