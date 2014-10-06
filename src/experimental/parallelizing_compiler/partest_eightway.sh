#!/bin/sh



for threads in `seq 1 8` 
do 
export REGTHREADS=$threads
# Eight constant tasks.
echo '(let ()  (define count (* 50 1000 1000))  (define (l1 x) (unless (zero? x) (l1 (sub1 x)))) (par-reset!) (time (par (l1 count) (l1 count) (l1 count)(l1 count)(l1 count)(l1 count)(l1 count)(l1 count))) (par-status))' | wavescript.threaded i 2> /dev/null | grep real
done
