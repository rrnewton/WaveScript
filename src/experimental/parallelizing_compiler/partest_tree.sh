#!/bin/sh



export REGOPTLVL=3
for threads in `seq 1 8` 
do 
export REGTHREADS=$threads
echo '(optimize-level 3)(let () (define (tree n)  (if (zero? n) 1   (call-with-values (lambda () (parmv (tree (sub1 n)) (tree (sub1 n)))) +)))  (par-reset!)  (printf "\n~s\n\n" (time (tree 23)))  (par-status))' | regiment.threaded i 2> /dev/null | grep real
done
