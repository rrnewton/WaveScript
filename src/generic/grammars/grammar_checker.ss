
;;;; .title Grammar Checker

;;;; This module allows me to write down and check grammars for the
;;;; mini-languages that occur between passes.

; [2005.10.15] For decent (medium) size programs analyze-grammar-failure is working *very* slowly.

; [2005.09.26]
; This compiler has grown large enough I'm going to check the output
; of passes to make sure they actually fit their announced grammars.
; This also forces me to make first class representations of grammars.


(module grammar_checker mzscheme
  (require (lib "include.ss")
	   "../../plt/iu-match.ss"
	   "../constants.ss" ;; For DEBUGMODE
           "../compiler_components/prim_defs.ss"
           "../compiler_components/type_environments.ss"
           "../../plt/chez_compat.ss"
	   (all-except "../util/helpers.ss" test-this these-tests)
           )
  (provide 
         check-grammar/backtrack
         check-grammar ;; version with no backtracking
	 analyze-grammar-failure 
         
         ;; Predifined Grammars
	 sugared_regiment_grammar
	 initial_regiment_grammar
	 ; elaborated_regiment_grammar
         basic_tml_grammar
;         tml_gradient_grammar
;         tml_letstored_grammar
         full_but_clean_tml
         
         test-grammar tests-grammar
         )

  (chezimports ;constants
               (except helpers   test-this these-tests)
;	       (except regiment_helpers   test-this these-tests)
	       )
; ======================================================================

;;; Main grammar checking entry points

  ;; [2005.09.26] This is limited right now.
  (define (check-grammar/backtrack origexpr grammar . initialprod)
  ;; .param origexpr is an sexpression
  ;; .param grammar is just a list of productions
  ;; .param initialprod indicates which production to use to start checking
    (define allvariants (list->set (map car grammar)))
    (define (cut-grammar p) (filter (lambda (prod) (eq? (car prod) p)) grammar))

  ;; This keeps track of how deep we are, for purposes of deciding which failure to report.
  (define current-depth 0)

  ;; Keeps track of the places where we fail, might be able to give some feedback.
  ;; Type: ([depth expr prod cont] ...)   where cont is the continuation to continue checking
  (define failure-stack '())

  (define (add-failure x p k)
;    (inspect `(FAILURE ,p ,current-depth ,x))
    (set! failure-stack (cons (list current-depth x p k)
			      failure-stack)))
  (define-syntax goingdeeper
    (syntax-rules ()
      [(_ e) (fluid-let ((current-depth (add1 current-depth))) e)]))
  (define-syntax fail
    (syntax-rules ()
      [(_ x p k)
       (begin
;	 (printf "Failing: ~a ~a depth ~a\n" x p current-depth)
	 (add-failure x p k)
	 ;; Signal failure and keep trying
	 (k #f))]))

  ;; This just goes through the grammar in order until it hits a match.
  (define (scangrammar expr prods k)
    ;(define (fail) (scangrammar expr (cdr prods)))
    ;(printf "Scanning ~a against ~a\n" expr (map car prods))
    (if (null? prods) (fail expr prods k)
	(match (car prods)
	  [(,lhs ,rhs)
	   (checkmatch expr rhs
		       (lambda (check)
			 (if check 
			     (k `(,lhs ,@check))
			     (scangrammar expr (cdr prods) k))))])))

  ;; This sees if an expression matches a given production.
  (define (checkmatch expr prod k)
    (match (list expr prod)
;		 [(,lhs ,type) (guard (basic-type? type))
;		  (if (check-basic expr type) #t
;		      (fail))]
      [(,x ,fun) (guard (procedure? fun))
	    (if (fun x)
		(if (atom? x)
		    (k (cons fun x))
		    (k fun)) ;'<fun>
		(fail x fun k))]
      [(,x (quote ,sym)) ;(guard (atom? x))
       (if (eq? x sym) 
	   (k `(quote ,sym))
	   (fail x `(quote ,sym) k))]
      [(,x (,p* ...)) ;; A list production
       (if (not (list? x))
	   (fail x p* k)
	   (goingdeeper (matchlist x p* k)))]
      [(,x #(,p* ...)) ;; A vector production, just convert to list
       (if (not (vector? x))
	   (fail x (list->vector p*) k)
	   (goingdeeper (matchlist (vector->list x) p* 
				   ;; Add to the continuation to convert back to vector
				   (lambda (x) (k (list->vector x))))))]
      [(,x ,p) (guard (memq p allvariants)) ;; A production-name
           (scangrammar x (cut-grammar p) k)]
      [(,_ ,p) (guard (symbol? p))
       (error check-grammar/backtrack "This is production-symbol is not bound: ~a" p)]
      [(,_ ,p)
       (error check-grammar/backtrack "Unknown kind of production in the grammar: ~a" p)]
      ))

  ;; This is for compound productions that have some structure to 'em.
  (define (matchlist ls p* k)
     (let listloop ((ls ls) (p* p*) (k k))
     (match (list ls p*)
	    [(() ()) (k ())]
	    [(,x ()) (fail x () k)]
	    [(() (,_ ,v)) (guard (eq? v '...)) (k ())] ;; If we ran out on a "..." that's ok.s
	    [(() ,x) (fail () x k)]
	    
	    [((,x ,lsnew ...)
	      (,fun ,p*new ...))
	     (guard (procedure? fun))
	     (if (fun x)
		 (goingdeeper (listloop lsnew p*new 
					(lambda (e) (if e (k (cons (if (atom? x) (cons fun x) fun) e))
							(k #f)))))
		 (fail x fun k))]

	    [((,x ,lsnew ...)
	      ((quote ,p) ,p*new ...))
	     ;(guard (atom? x))
	     (if (eq? x p)
		 (goingdeeper (listloop lsnew p*new (lambda (e) (if e (k (cons `(quote ,p) e)) (k #f)))))
		 (fail x `(quote ,p) k))]

	    ;; Here we greedily eat up everything when we have a "..." ("*" BNF notation):
	    [((,x ,lsnew ...)
	      (,p ,elipses ,p*new ...)) (guard (eq? elipses '...))
	      (goingdeeper (checkmatch x p
		 (lambda (check)
		   (if check
		       (goingdeeper (listloop lsnew p* (lambda (e) (if e (k (cons check e)) (k #f)))))
		       (goingdeeper (listloop ls p*new k))))))]

	    ;; Named production:
	    [((,x ,lsnew ...)
	      (,p ,p*new ...))
	     (guard (memq p allvariants))
	     (scangrammar x (cut-grammar p)
			  (lambda (scan)
			    (if scan
				(goingdeeper (listloop lsnew p*new (lambda (e) (if e (k (cons scan e)) (k #f)))))
				(fail x p k))))]
	    [((,x ,lsnew ...)
	      (,p ,p*new ...))
	     (guard (symbol? p))
	     (error check-grammar/backtrack "This is production-symbol is not bound: ~a" p)]

	    ;; [sub]List pattern:
	    [((,x ,lsnew ...)
	      ((,subp* ...) ,p*new ...))
	     (if (not (list? x))
		 (fail x subp* k)
		 (goingdeeper (listloop x subp*
					(lambda (sub)
					  (if sub
					      (listloop lsnew p*new (lambda (e) (if e (k (cons sub e)) (k #f))))
					      (fail x subp* k))))))]
	    [,other (error 'check-grammar/backtrack:match-list "unmatched sexp/pattern pair: ~a\n" other)]
	    )))
;  (inspect origexpr)

  (let ((result 
	 (scangrammar origexpr (if (null? initialprod)
;			 (if (assq 'PassInput grammar)
;			     (begin ;; This is cheesy but convenient for me:
;			       (printf "Defaulting to using 'PassInput as starting production for grammar check.\n")
;			       (cut-grammar 'PassInput))
			     ;; Otherwise we allow a match against any production in the grammar:
			 grammar
			 (cut-grammar (car initialprod)))
		(lambda (e) e))))
    (if (not result)
	(begin (set-top-level-value! 'failure-stack failure-stack)
	       (set-top-level-value! 'current-parse origexpr)
	       (printf "Grammar check failed.  Stored failure trace in global var \"failure-stack\", and program in \"current-parse\"\n")
	       (printf "Run (analyze-grammar-failure failure-stack) to see what went wrong.\n")
	       #f)
	result)
   ))

;; This function allows one to try to figure out where the problem was
;; when a grammar check fails.  This is a total hack and operates
;; according to an arbitrary heuristic I made up.  At some point I
;; should take a more disciplined approach to this grammar checking
;; business.
(define (analyze-grammar-failure grammar-failure)

    (let* ((count (length grammar-failure))
	   (progressbar (display-progress-meter count)))
      (printf "\nAnalyzing ~a failure scenarios.\n" count)
      (let ((winner-list
	     (filter id 
	       (map (lambda (ls)
		      (match ls
			[(,d ,x ,p ,k)
			 (let ((reconstructed (k 'FAIL)))
			   (if reconstructed
			       (let ((failcount (length (deep-all-matches (lambda (x) (eq? x 'FAIL)) reconstructed))))
					;(printf "Failcount ~a\n" failcount)
				 (if (= 0 failcount)
					;#f
				     ;; This is not a failure!
				     (error 'analyze-grammar-failure 
					    "Correct Parsing!\n ~s \n Correct Parsing! \n"
					    reconstructed)
				     ;; This is my random scoring heuristic:
				     ;; We want a large context and a small expression:
				     (let ((score (- (count-nodes reconstructed)
							(* 30 (count-nodes x)))
						  ;failcount
						     ))
				       (progressbar)
				       `(,score ,d ,x ,p ,reconstructed))))
			       (begin (progressbar)
				      ;; We couldn't do anything at all with it, failed reconstruct:
				      ;; TODO: FIXME: WHEN DOES THIS HAPPEN, WHY?
				      #f))
			   )]))
		 grammar-failure))))

	;; First we sort minimizing depth:
	;; Weird, but this seems heuristically to work ok:
;; [2005.10.26] Disabling again... it's hard to say with these heuristics:
;	(set! winner-list (sort! (lambda (x y) (< (cadr x) (cadr y))) winner-list))
	
	(printf "\nMost likely failed parsing: \n")
	(set! winner-list (sort! (lambda (x y) (> (car x) (car y))) winner-list))
	
;	(pretty-print winner-list)

	(let userloop ((i 0) (ls winner-list) (past ()))
	  (if (null? ls) (printf "\n No more grammar failures to analyze.\n")
	      (match (car ls)
		[(,score ,d ,x ,p ,context)
		 (printf "\n~a: At failure depth ~a, expression below did not satisfy ~a (context score ~a)\n    " i d p score)
		 (parameterize (;(pretty-standard-indent 5)  ;; <- This didn't have the desired effect 
				;(pretty-initial-indent 5)   ;; And isn't in plt anyway...
				(print-level 3)
				(print-length 5))
		   (pretty-print x))
		 (let menuloop ()
		   (printf "  Press n(next), p(previous), c(failure context), q(quit): ")
		   (let readloop ()
		     (case (read)
		       [(q) (void)]
		       [(n) 
			(if (null? ls)
			    (begin (printf "No more failure scenarios.")
				   (userloop i ls past))
			    (userloop (add1 i) (cdr ls) (cons (car ls) past)))]
		       [(p) 
			(if (null? past)
			    (begin (printf "No more previous failure scenarios.\n")
				   (userloop i ls past))
			    (userloop (sub1 i) (cons (car past) ls) (cdr past)))]
		       [(c) 
			(printf "\nContext:\n")
			(parameterize (;(pretty-standard-indent 5)
				       ;(pretty-initial-indent 5)
                                       )
			  (pretty-print context))
			(menuloop)]
		       [else (printf "\nInvalid input.\n") (readloop)]
		       )))]))))))


; =================================================================================


;; Trying again: no backtrack
;; .param context - this is usually the name of the pass for which check-grammar is called
(define (check-grammar context origexpr grammar . initialprod)
  ;; expr is an sexpression
  ;; grammar is just a list of productions
  (define allvariants 
    (begin 
      ;; Some defense:
      (ASSERT list? grammar)
      (DEBUGASSERT (curry andmap pair?) grammar)
      ;(printf "GRAMMAR ~s\n" grammar)
      (ASSERT (or (null? initialprod) 
		  (and (list? initialprod) (symbol? (car initialprod)) 
		       (null? (cdr initialprod)))))
      (list->set (map car grammar))
      ))
  (define (cut-grammar p) (filter (lambda (prod) (eq? (car prod) p)) grammar))

  (define (fail x p k)
    (define-top-level-value 'grammar-context (k 'FAIL))
    (define-top-level-value 'grammar-original origexpr)
    (define-top-level-value 'grammar-failed x)
    (warning 'check-grammar
	   (++ (format "in ~a: " context)
	       "could not parse expr ~s with production/pattern ~s\n  "
	       "Context stored in 'grammar-context', look at the location of FAIL.\n"
	       "Original expression in 'grammar-original'.\n" 
	       "Failed subexpression in 'grammar-failed'.\n")
	   x p)
    (IFCHEZ 
     ;; In batch mode don't go into REPL:
     (unless (and (top-level-bound? 'REGIMENT-BATCH-MODE)
		  (top-level-value 'REGIMENT-BATCH-MODE))       
       (new-cafe)) 
     (void))
    (error 'check-grammar "")
    )
  
  ;; This matches an expression against a variant name (e.g. Expr, Var, etc)
  (define (match-variant expr variantname k)
    (if (not (memq variantname allvariants))
	(error 'check-grammar:match-variant
	       "there are no productions for the variant '~s in the input grammar" variantname))
    (let loop ([pats (map cadr (cut-grammar variantname))])
      (if (null? pats) 
	  (fail expr variantname k)
	   ;; If it matches this one, keep going deeper.
	   (if (matchhead? expr (car pats))
	       (match-pattern expr (car pats)
			      (lambda (x) (k `(,variantname . ,x))))
	       (loop (cdr pats))))))

  ;; This sees if an expression matches a given production.
  (define (matchhead? expr pat)
    (match (list expr pat)
      [(,x ,fun) (guard (procedure? fun)) (fun x)]
      [(,x (quote ,sym)) (eq? x sym)]
      [(() ()) #t]      
      [(() (,p ,ellipses)) (guard (eq? ellipses '...)) #t]
      [(,x (,p* ...)) ;; A list production
       (and (list? x) (not (null? x))
	    (matchhead? (car x) (car p*)))]

      [(,x #(,p* ...)) ;; A vector production
       (and (vector? x) (matchhead? (vector->list x) p*))]

      ;; This is an alias between one production and another:
      [(,x ,p) (guard (symbol? p) (memq p allvariants))
       (matchhead-any? x (cut-grammar p))]
      [(,_ ,p) ;(guard (symbol? p))
       (error 'check-grammar:matchhead? "This pattern is not handled, no production for symbol: ~a" p)]
      ))

  ;; Does its match any of these productions?
  (define (matchhead-any? expr prods)
    (ormap (lambda (p) (matchhead? expr (cadr p))) prods))

  ;; We know the expression must match this pattern, dig deeper inside it.
  (define (match-pattern expr pat k)
    (match (list expr pat)
;		 [(,lhs ,type) (guard (basic-type? type))
;		  (if (check-basic expr type) #t
;		      (fail))]
      [(,x ,fun) (guard (procedure? fun))   (ASSERT (fun x))
       (k (cons fun x))]
      [(,x (quote ,sym))                    (ASSERT (eq? x sym))
       (k `(quote ,sym))]
      ;; A list production
      [(,x (,p* ...)) (matchlist x p* k)]
      [(,x #(,p* ...)) 
       (if (vector? x)
	   (matchlist (vector->list x) p* k)
	   (fail x (list->vector p*) k))]

      ;; This is an alias between one production and another:
      [(,x ,p) (guard (symbol? p) (memq p allvariants)) (match-variant x p k)]
      [(,_ ,p) ;(guard (symbol? p))
       (error 'check-grammar:matchhead-pattern "This pattern is not handled: ~a" p)]))

  ;; This is for compound productions that have some structure to 'em.
  (define (matchlist ls p* k)
    (let listloop ((ls ls) (p* p*) (k k))
     (match (list ls p*)
	    [(() ()) (k ())]
	    [(,x ()) (fail x () k)]

	    [(() (,_ ,v)) (guard (eq? v '...)) (k ())] ;; If we ran out on a "..." that's ok.
	    [(() ,x) (fail () x k)]
	    
	    ;; Here the head of the list is a function:
	    [((,x ,lsnew ...)
	      (,fun ,p*new ...))
	     (guard (procedure? fun))
	     (if (fun x)
		 (listloop lsnew p*new 
			   (lambda (e) (k (cons (if (atom? x) (cons fun x) fun) e))))
		 (fail x fun k))]

	    ;; The head of the list is a literal symbol.
	    [((,x ,lsnew ...)
	      ((quote ,p) ,p*new ...))	     
	     (if (eq? x p)
		 (listloop lsnew p*new (lambda (e) (k (cons `(quote ,p) e))))
		 (fail x `(quote ,p) k))]


	    ;; When we have an elipses, we cut off the end and match it, then we do the elipses.
	    [((,x* ...)
	      (,p ,ellipses ,p*new ...)) (guard (eq? ellipses '...) (not (null? p*new)))
	      ;; First match the end:
	      (let ([len1 (length x*)]
		    [len2 (length p*new)])
	      (unless (>= len1 len2)
		(fail x* `(,p ,ellipses ,p*new ...) k))
	      (listloop (list-tail x* (- len1 len2)) p*new
			(lambda (tail)
			  (listloop (list-head x* (- len1 len2))
				    `(,p ,ellipses)
				    (lambda (head)
				      (k (append head tail)))))))]
	    ;; This variant is for a trailing ellipses:
	    [((,x ,lsnew ...)
	      (,p ,elipses)) (guard (eq? elipses '...))
	      (match-pattern x p
			     (lambda (check)
			       (listloop lsnew p* (lambda (e) (k (cons check e))))))]

	    ;; Named production:
	    [((,x ,lsnew ...)
	      (,p ,p*new ...))
	     (guard (symbol? p) (memq p allvariants))
	     (match-variant x p
			  (lambda (scan)
			    (if scan
				(listloop lsnew p*new (lambda (e) (k (cons scan e))))
				(fail x p k))))]
	    [((,x ,lsnew ...)
	      (,p ,p*new ...))
	     (guard (symbol? p))
	     (error check-grammar "This is production-symbol is not bound: ~a" p)]

	    ;; [sub]List pattern:
	    [((,x ,lsnew ...)
	      ((,subp* ...) ,p*new ...))
	     (if (not (list? x))
		 (fail x subp* k)
		 (listloop x subp*
			   (lambda (sub)
			     (if sub
				 (listloop lsnew p*new (lambda (e) (k (cons sub e))))
				 (fail x subp* k)))))]
	    )))


  (if (null? initialprod) (error 'check-grammar "must have initial production."))
  (match-variant origexpr (car initialprod)
		 (lambda (e) e)))


;; This contains actual grammar definitions (used in unit tests below).
(IFCHEZ 
 (include "generic/grammars/grammars.ss")
 (include "grammars.ss"))

; =======================================================================
;;; Unit tests.

;; Unit tests.
(define-testing these-tests
  `([(check-grammar/backtrack '(set! foo 3) basic_tml_grammar 'Expr) ,list?]
    [(check-grammar/backtrack '(ext-set! (tok foo 3) storedvar 4) basic_tml_grammar 'Expr) ,list?]
    [(check-grammar/backtrack '(let ((x 4)) (let ((y 5)) 3)) basic_tml_grammar 'Expr)   ,list?]
    [(check-grammar/backtrack '(nodepgm (tokens)) basic_tml_grammar) ,list?]
    [(car (check-grammar/backtrack '(tok1 subind () (stored) 333) basic_tml_grammar)) TokBinding]
    [(check-grammar/backtrack '(program (bindings) (nodepgm (tokens))) basic_tml_grammar) ,list?]
    [(check-grammar/backtrack '(program (bindings (x '3)) (nodepgm (tokens))) basic_tml_grammar) ,list?]
    [(check-grammar/backtrack '(program (bindings) (nodepgm (tokens
						   (tok1 subind () (stored) 333)
						   ))) basic_tml_grammar)
     ,list?]
    ["Testing elipses followed by something"
     (check-grammar/backtrack '(testfoo a b c 3) 
		    `([Test ('testfoo Var ... Num)]
		      [Num ,integer?]
		      [Var ,symbol?]))
     ,list?]

    ["Run check on example output of cleanup-token-machine: "
     (check-grammar/backtrack '(program
		      (bindings)
		      (nodepgm
		       (tokens
			(SOC-start subtok_ind () (stored) (void))
			(node-start subtok_ind () (stored) (printf '"woot"))))) basic_tml_grammar)
     ,list?]


    #; 
    ["Test a failed grammar check."
     (check-grammar/backtrack '(dbg "test" '1 '2 '3) basic_tml_grammar 'Expr)
     ????]


    ;; [2006.05.03] Duplicate the same tests for check-grammar:
    [(check-grammar "" '(set! foo 3) basic_tml_grammar 'Expr) ,list?]
    [(check-grammar "" '(ext-set! (tok foo 3) storedvar 4) basic_tml_grammar 'Expr) ,list?]
    [(check-grammar "" '(let ((x 4)) (let ((y 5)) 3)) basic_tml_grammar 'Expr)   ,list?]
    [(check-grammar "" '(nodepgm (tokens)) basic_tml_grammar 'NodePgm) ,list?]
    [(car (check-grammar "" '(tok1 subind () (stored) 333) basic_tml_grammar 'TokBinding)) TokBinding]
    [(check-grammar "" '(lang '(program (bindings) (nodepgm (tokens)))) basic_tml_grammar 'PassInput) ,list?]

    [(check-grammar "" '(lang '(program (bindings (x '3)) (nodepgm (tokens)))) 
		     basic_tml_grammar 'PassInput) ,list?]
    [(check-grammar "" '(lang '(program (bindings) (nodepgm (tokens (tok1 subind () (stored) 333)))))
		     basic_tml_grammar 'PassInput)
     ,list?]

    ["Testing elipses followed by something"
     (check-grammar "" '(testfoo a b c 3) 
		    `([Test ('testfoo Var ... Num)]
		      [Num ,integer?]
		      [Var ,symbol?]) 'Test)
     ,list?]
    ["Run check on example output of cleanup-token-machine: "
     (check-grammar "" '(lang '(program
		      (bindings)
		      (nodepgm
		       (tokens
			(SOC-start subtok_ind () (stored) (void))
			(node-start subtok_ind () (stored) (printf '"woot")))))) 
		     basic_tml_grammar 'PassInput)
     ,list?]




    ))

(define-testing test-this (default-unit-tester
		    "grammar_checker: this is my by-hand grammar checker for pass input/output"
		    these-tests))

;; Unit tester.
(define test-grammar test-this)
(define tests-grammar these-tests)

) ;; End module.

; (require grammar_checker) (test-grammar)