

(chez:module alpha_lib_scheduler_simple 
	(
	 run-alpha-simple-scheduler
	 )

	(import scheme)
;temp;	(import simulator_alpha_datatypes)
	(import alpha_lib)
;	(import simulator_alpha)

	;; Consider compiling simulator code in opt 3 when it's stable.
;	(eval-when (compile load eval)
;	  (optimize-level (IFDEBUG 2 2)))
	
	(include "generic/alpha_lib_scheduler_simple.ss")
)
