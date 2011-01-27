



(import (primitives dump-interactive-heap current-require-path compiler-switches) 
	(rnrs)(rnrs r5rs (6)) (rnrs mutable-pairs (6)) (rnrs mutable-strings (6)) 
	(main_r6rs) (main))

;(compiler-switches)

(dump-interactive-heap "larc_r6rs.heap")
;(dump-heap "larc.heap" (lambda args (load "newloads.ss") (exit)))

