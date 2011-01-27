#!/bin/sh



export REGOPTLVL=2
for threads in 1 #2 3 4
do 
export REGTHREADS=$threads
#echo '(optimize-level 3)(let () (define (tree n)  (if (zero? n) 1   (call-with-values (lambda () (parmv (tree (sub1 n)) (tree (sub1 n)))) +)))  (par-reset!)  (printf "\n~s\n\n" (time (tree 23)))  (par-status))' | regiment.threaded i #2> /dev/null | grep real
#echo '(optimize-level 3)(let () (define (tree n)  (if (zero? n) 1   (call-with-values (lambda () (pcall (tree (sub1 n)) (tree (sub1 n)))) +)))  (par-reset!)  (printf "\n~s\n\n" (time (tree 23)))  (par-status))' | regiment.threaded i #2> /dev/null | grep real


echo '(optimize-level 3)  (let ()  (define (tree n)   (if (zero? n) 1     (pcall + (tree (sub1 n)) (tree (sub1 n)))))  (printf "\n~s\n\n" (time (tree test-depth))) (par-status))' | regiment.threaded i #2> /dev/null | grep real
done
