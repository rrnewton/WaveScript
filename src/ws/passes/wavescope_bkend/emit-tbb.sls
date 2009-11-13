;;;; [2009.11.07] This is a specialization of emit-c2 (wsc2) that
;;;; produces code to be run with a TBB scheduler.

;;;; .author Ryan Newton

(library (ws passes wavescope_bkend emit-java)
  (export <emit-tbb>
	  )
  (import (except (rnrs (6)) error) (except (rnrs r5rs) force delay)
	  (rnrs mutable-strings)
	  (ws compat compat)
	  (ws common)
	  (ws passes wavescope_bkend insert-refcounts)
	  (ws passes wavescope_bkend emit-c)
	  (ws compiler_components c_generator)
	  (ws util bos_oop) 
	  (ws compiler_components c_generator)
	  (ws passes wavescope_bkend emit-c2)
	  ;(ws util rn-match) ;; TEMPTOGGLE
	  )

  ;; ACK -- we need mix-ins here because we'd like to be able to
  ;; combine the tbb backend with different GC related backends like
  ;; emitC2-zct.
  
  (define-class <emit-tbb> (<emitC2>) (wserror-acc import-acc))
  
  (__spec GenWorkFunction <emitC2-base> (self name arg vqarg argty code down*)
     (define _arg (Var self arg))
     (define _argty (Type self argty))
     (define extra (map (lambda (x) (list x ", ")) (ExtraKernelArgsHook self)))
     (values
      (make-lines `("void ",(Var self name) "(",extra ,_argty" ",_arg"); // Iter prototype\n"))
      (make-lines 
       (list (block `("void ",(Var self name) "(",extra ,_argty" ",_arg")")
		    (list
		     "char "(Var self vqarg)";\n"
		     (IterStartHook self name arg argty down*)
		     (lines-text code)
		     (IterEndHook self name arg argty down*)))
	     "\n"))))
)

