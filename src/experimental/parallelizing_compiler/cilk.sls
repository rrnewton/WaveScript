
(library (cilk)
 (export 
     cilk 
     ;; Reexport these:
     init-par shutdown-par
     )
  
 (import (rnrs (6))
;         (par6)
         (par5)
	 (only (scheme) gensym iota)
	 )

;; A syntax for writing Cilk-like programs.

;; HAVING TROUBLE PUTTING THIS IN HERE INSTEAD OF cilk_test.ss

) ;; End library
