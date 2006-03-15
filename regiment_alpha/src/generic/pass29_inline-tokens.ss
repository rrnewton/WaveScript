;;;; .title Pass 29: Inline Tokens
;;;; .author Ryan Newton

;; [2005.10.26] Gotta do this at some point.  The output of
;; deglobalize is *begging* for it.


;; [2006.03.14] As a stop-gap measure I'm doing a quick little eta
;; reduction when we see:

;;   [tok1 (args ..) (call tok2 args ...)]

;; We simply consider tok1 an alias for tok2.


(module pass29_inline-tokens mzscheme

  (require (only "../generic/constants.ss" chezimports ASSERT)
           "../plt/iu-match.ss"
	   "../plt/prim_defs.ss"
           (all-except "../plt/tml_generic_traverse.ss" test-this these-tests)
           (all-except "../plt/helpers.ss" test-this these-tests)
           (all-except "../plt/regiment_helpers.ss" test-this these-tests))
  
  (provide inline-tokens test29 test-inline )

  (chezimports )

; =================================================================================

(define inline-tokens
  (let ()


    ;; Is a tokbind subject to eta-reduction?
    (define tokbind-eta?
      (lambda (tb)
	(mvlet ([(tok id args stored constbinds body) (destructure-tokbind tb)])
	  (if (and (null? stored) (null? constbinds))
	      (match body 
		;; Only works for subtokind zero currently!!
		[(call (tok ,newtok 0) ,rands ...)
		 (if (equal? args rands)
		     ;; This can be eta reduced:
		     `(,tok ,newtok)
		     #f)]
		[,otherwise #f])
	      #f))))

    ;; This performs the substition for the eta-able token names.
    (define (subst-calls tb subst)
      (mvlet ([(tok id args stored constbinds body) (destructure-tokbind tb)])
        (ASSERT (null? constbinds))
	`[,tok ,id ,args (stored ,@stored )
	  ,(tml-generic-traverse
	    ;; Main transformer:
	    (lambda (x autoloop)
	      (match x
		[(call (tok ,orig ,subid) ,[autoloop -> args] ...)
		 (guard (or (integer? subid) (symbol? subid))) ;; Don't want to prevent the evaluation of the subid expression
		 (let ([entry (assq orig subst)])
		   (if entry
		       `(call (tok ,(cadr entry) 0) ,args ...)
		       `(call (tok ,orig ,subid) ,args ...)))]		 
		[,o (autoloop o)]))
	    ;; Result fuser:
	    (lambda (xs k) (apply k xs))
	    body)]))

    ;; [2006.03.15] Doesn't handle loops; will diverge!
    (define (transitive-closure substs)
      (letrec ([loop (lambda (sym)
		       (let ([entry (assq sym substs)])
			 (if entry (loop (cadr entry))
			     sym)))])
	(map (lambda (pr) (list (car pr) (loop (cadr pr))))
	  substs)))

    ;; Main pass body:
    (lambda (prog)
      (let ([escaped (tml-potential-escaped-tokens prog)])
	(printf "Escaped: ~a\n" escaped)
	(match prog
	  [(,lang '(program (bindings ,constbinds ...)
		     (nodepgm (tokens ,nodetoks ...))))
	   ;; Substs can only happen for names that have not escaped.
	   ;; Also the system entry points SOC-start/node-start cannot be eliminated!
	   ;; (Maybe the should be considered 
	   (let ([substs (transitive-closure 
			  (filter (lambda (entry)
				   (and entry
					(not (memq (car entry) '(SOC-start node-start))) ;; Cannot eta reduce these!
					(not (memq (car entry) escaped))))
			   (map tokbind-eta? nodetoks)))])
	     (printf "Substs: \n" ) (pretty-print substs)
	     `(,lang '(program (bindings ,constbinds ...)
			(nodepgm 
			    (tokens 
			      ;; Do the substitions:
			      ,@(map (lambda (tb) (subst-calls tb substs))
				  ;; But we also delete tokbinds that have been eta'd away.
				  (filter (lambda (tb) (not (assq (car tb) substs)))
				    nodetoks)
				  ))))))])))
    ))
  
  (define these-tests
    `( 
      ["Does it do the basic eta transform?"
       (,deep-assq 'tokens 
		  (inline-tokens
		   '(lang '(program (bindings)
			     (nodepgm (tokens 
					[a () (call (tok b 0))]
					[b () 99]
					[node-start () (call (tok a 33))]))))))
       (tokens
         [b subtok_ind () (stored) 99]
         [node-start subtok_ind () (stored) (call (tok b 0))])]


      ["Don't inline SOC-start / node-start"
       (,deep-assq 'tokens 
		  (inline-tokens
		   '(lang '(program (bindings)
			     (nodepgm (tokens 
					[a () 99]
					[node-start () (call (tok a 0))]
					[SOC-start () (call (tok a 0))]
					))))))
       (tokens
	 (a subtok_ind () (stored) 99)
	 (node-start subtok_ind () (stored) (call (tok a 0)))
	 (SOC-start subtok_ind () (stored) (call (tok a 0))))]

      ["Make sure it does the transitive closure of replacements."
	(,deep-assq-all 'call
			(inline-tokens
			 '(lang '(program (bindings)
				   (nodepgm (tokens 
					      [a () 99]
					      [b () (call (tok a 0))]
					      [c () (call (tok b 0))]
					      [node-start () (call (tok c 0))]
					      [SOC-start ()  (call (tok b 0))]
					      ))))))
	,(lambda (ls)
	   (equal? ls	    
	    ;; Should not get : ((call (tok b 0)) (call (tok a 0)))
	    '((call (tok a 0)) (call (tok a 0)))))]
      ))
  
  (define test29 (default-unit-tester "Pass29: Inline token handlers" these-tests))
  (define test-inline test29)

) ;; End module.

;(require pass29_inline-tokens)
