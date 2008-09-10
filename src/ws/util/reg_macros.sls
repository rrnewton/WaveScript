#!r6rs

;;;; .title Macros for the Regiment codebase.
;;;; .authore

;;;; [2006.03.10] <br> 
;;;; These are bits of syntactic extension that are used throughout the codebase.
;;;; I am now pulling them out and centralizing them here.  Because: 
;;;;<br>   1) They are used in a wide range of places.
;;;;<br>   2) To read Regiment code, you need to know what the syntax means.  
;;;;          Look here and read all these forms.

;;;; <br><br>
;;;;  NOTE: The biggest syntactic extension of all used in Regiment is
;;;;  'match' in match.ss.  It's the pattern matcher and you'll need
;;;;  to know how it works to work with the compiler passes (or much
;;;;  of the rest of the code-base for that matter).

(library (ws util reg_macros)
  (export 
      eq-any?
      for grep rep
      cheap-fluid-let
      let-match match?
      mvlet
      match-lambda match-lambda-helper
      ** ^ ;; Exponentiation
      define-id-syntax
      rec 
      ;;reg:define-struct ;; Moved to constants.ss 
      apply-ordered

      parameterize-IFCHEZ      
      map-inlined map-tail-inlined

      time-accum
      time-accum-report
      time-accum-buf

      let-par

      let/cc let/ec

      define-testing
      debug-return-contract
      )
  (import 
   (except (rnrs (6)) error)
   (for (ws compat compat) expand run)
   (for (ws globals) expand run)
   (ws util iu-match)
;    (lib "include.ss")
;    "../../plt/iu-match.ss"
; ;   (require-for-syntax "../../generic/constants.ss")
;    "../../generic/constants.ss"
;    "../../plt/chez_compat.ss"
;    "../../generic/constants.ss"
   )
  
  ;; These provide extra information for Chez:
#;
  (chezprovide (match-lambda match-lambda-helper)
	       let/cc)
  
  ;===============================================================================

  ;; multiple-value let
  (define-syntax mvlet (identifier-syntax let-values))
#;
  (cond-expand
   [chez
    (define-syntax mvlet
      (lambda (x)
	(define domvlet
	  (lambda (bindings ids tmps body)
	    (if (null? bindings)
		`((,#'lambda ,ids ,@body) ,@tmps)
		(syntax-case (car bindings) ()
			     [(*ids expr)
			      (with-syntax ([*tmps (generate-temporaries #'*ids)])
				(with-syntax ([body (domvlet (cdr bindings)
							     (append #'*ids ids)
							     (append #'*tmps tmps)
							     body)])
				  #'(call-with-values
					(lambda () expr)
				      (lambda *tmps body))))]))))
	(syntax-case x ()
		     [(_ (((id ...) expr) ...) form ...)
		      (andmap (lambda (ls) (andmap identifier? ls))
			      #'((id ...) ...))
		      (domvlet #'(((id ...) expr) ...) '() '() #'(form ...))])))]
   [(or plt larceny)
    ;; In PLT this is just the same as let-values.
    (define-syntax mvlet
      (syntax-rules ()
	[(mvlet stuff ...) (let-values stuff ...)]))])

  ;; This could surely be more efficient.
  ;; Especially because excess allocation is extra bad in a parallel setting.
  (define-syntax let-par
    (syntax-rules ()
      [(_ ([lhs* rhs*] ...) bod* ...)
       (let-match ([(,lhs* ...) (par rhs* ...)]) bod* ...)]))
  
#;  
  (IFCHEZ
   (define-syntax reg-include
     (syntax-rules ()
       [(_ fn) (include fn)]))
   (define-syntax reg-include
     (syntax-rules ()
       [(_ fn) (include (build-path "../../" fn))])))
  
  (define-syntax rec
    (syntax-rules ()
      ((_ x e) (letrec ((x e)) x))))

  (define-syntax let/cc
    (syntax-rules ()
      ((_ V B0 B ...) (call/cc (lambda (V) B0 B ...)))))
  (define-syntax let/ec
    (syntax-rules ()
      [(_ v exp ...)
       (call/ec (lambda (v) exp ...))]))

#;
  (cond-expand
   [(or chez larceny)
    (define-syntax let/cc
	  (syntax-rules ()
	    ((_ V B0 B ...) (call/cc (lambda (V) B0 B ...)))))]
   [plt])

;; [2007.04.20] This hack doesn't seem to yield any benefit vs. doing
;; a (memq)... Chez must be doing the right thing for the memq in a constant list.
(define-syntax eq-any?
  (syntax-rules ()
    [(_ x arg* ...)  (let ([y x]) (or (eq? y arg*) ...))]))
;; Turn this one (memq version) on to compare:
;(define-syntax eq-any? (syntax-rules () [(_ x arg* ...) (memq x (list arg* ...))]))

;; Repeatedly execute an expression some number of times:
(define-syntax rep
  (syntax-rules ()
    [(_ reps exp ...)
     (let loop ([n reps])
       (if (< n 1)
           (void)
           (begin exp ...
                  (loop (- n 1)))))]))

;; [2005.10.16]  I hate 'do' syntax, for loops are much cleaner.
;; For now we only allow fixnum indices:
(define-syntax for
  (syntax-rules (= to)
    [(_ v = start to end expr ...)
     (let ((s start)
	   (e end))
       (do ([v s (fx+ v 1)])
	   ((fx>? v e))
	 (let ()
	   expr ...)))]))

(define-syntax ^
  (lambda (x)
    (syntax-case x ()
		 [id (identifier? #'id) #'expt]
		 [(_ a b)  #'(expt a b)])))


;; [2005.10.23] Should have used these more before
;; Only works for one argument functions currently:
;; Need to use syntax-case I believe.
(define-syntax match-lambda-helper
  (lambda (x) 
    (syntax-case x (unquote)
      ; When we've no patterns to match, produce a real lambda:		 
      [(_ () (V ...) E ...) #'(lambda (V ...) E ...)]
      [(_ (P1 P ...) (V ...) E ...)
       #'(match-lambda-helper (P ...) (V ... tmp)
	   (match tmp
             [P1 (begin E ...)]
	     [,other (error 'match-lambda "unmatched object ~s for pattern ~s" other #'P1)]))])))
(define-syntax match-lambda 
  (lambda (x)
    (syntax-case x (unquote)
      [(_ (Pat ...) Expr ...)
       #'(match-lambda-helper (Pat ...) () Expr ...)
       ])))
;(expand '((match-lambda (x y) #t) 'x 'y))
;(expand '(match-lambda (,x ,y) #t))
;((match-lambda ((,x ,y) #(,z) ,w) (list x z w)) `(a b) #(3) 4)


;; This allows pattern matching in the LHSs of let's.
(define-syntax let-match
  (lambda (x)
    ;; [2007.04.20] Does this work??
    (IFCHEZ (import rn-match) (begin))
    (syntax-case x (unquote)
      [(_ () Body ...)
       #'(begin Body ...)]
      [(lm ([Pat Exp] Rest ...) Body ...)
       #'(match Exp
	   [Pat (let-match (Rest ...) Body ...)]
	   [,other (error 'let-match "unmatched object.\n  Datum: ~s\n  Syntax-location ~s\n" 
			  other #'Exp)]
	   )])))
;(expand '(let-match () 3))
;(expand '(let-match ([,x 3]) x))
;(let-match ([(,x ,y ...) (list 3 4 5 6)] [,z 99]) (list y z))

(define-syntax match?
  (syntax-rules ()
    [(_ x pat) (match x [pat #t] [,else #f])]))

;; [2007.10.20] A lot of the time we know there won't be any
;; continuations invoked within the dynamic extent of a fluid-let.  In
;; this case the dynamic-wind based version is inefficient.
(define-syntax cheap-fluid-let
  (syntax-rules ()
    [(_ ([lhs* rhs*] ...) bod* ...)
     (fluid-let ([lhs* rhs*] ...) bod* ...)
     ]))

;;<br> [2005.10.05]
;;<br>  Evaluate expression and mask output by search string.  
;;<br>  NOTE: Currently just does string match, not regexp.
(define-syntax grep
  (syntax-rules ()
    [(_ pat exp)
     (let ([str (open-output-string)]
	   [searchpat pat]
	   [leftovers ""])
       (let* ([guarded-display (lambda (s)
				 (when (substring? searchpat s)
				   (display s (console-output-port))
				   (newline (console-output-port))))]
	      [print (lambda ()		      
		       (let ((chunks (string-split 
				     (string-append leftovers (get-output-string str))
				     #\newline)))
;			(if (> (length chunks) 1)  (printf "\nGot chunks: ~s\n" chunks))
			(cond 
			 [(null? chunks) (void)]
			 [else (for-each guarded-display (rdc chunks))
			       (set! leftovers (rac chunks))])))]
	      [eng (make-engine (lambda () 
				  (parameterize ((current-output-port str)
						 (console-output-port str))
				    exp)))])
	 (let loop ((eng eng))
	   (eng 100
		(lambda (ticks val) (print) val (guarded-display leftovers))
		(lambda (neweng) 
		  (print) 
		  (loop neweng))))))]))
  

  ;===============================================================================

;; [2005.10.29] In scheme order of evaluation of operands is not specified.
;; This forces left-to-right evaluation.
#;
(define-syntax apply-ordered
  (lambda (x)
    (syntax-case x ()
      [(_ f x ...)
       (begin 
	 (let apply-ordered-loop ([args (reverse #'(x ...))] [newargs '()] [binds '()])
	   (if (null? (args)
	       (with-syntax ((b binds) (na newargs))
		 #'(let*foo b (f . na)))
	       (let ((tmp #'tmp))
		 (apply-ordered-loop (cdr args)				     
				     (cons tmp newargs)
				     (with-syntax ((t tmp) (a (car args)))
				       (cons ;#`(#,tmp #,(car args))
					     #'(t a)
					     newargs
					     ))))))))])))

(define-syntax apply-ordered
  (lambda (x)
    (syntax-case x ()
      [(appord f x ...)
       (with-syntax (((tmp ...) (map 
				    (lambda (_) (datum->syntax #'appord (gensym "tmp")))
				  (syntax->datum #'(x ...)))))
	 #'(let* ((tmp x) ...) (f tmp ...)))])))

;; '**' is a string-append shorthand
#;(define-syntax **
  (lambda (x)
    (syntax-case x ()
		 [id (identifier? #'id) #'string-append]
		 [(_ E ...) #'(string-append E ...)])))
(define ** string-append)  ;; Trust the inliner. [2006.03.02]

;; This is used for defining convenient shorthands than need no parentheses!
(define-syntax define-id-syntax  
  (lambda (x)
    (syntax-case 
	x ()
	[(_ v e)
	 #'(define-syntax v 
	     (lambda (x)
	       (syntax-case x ()
			    [id (identifier? #'id) #'e]
			    [(id x (... ...)) (identifier? #'id)
			     #'(e x (... ...))])))])))

;; Evaluate subexpressions but suppress output.
;; TODO: Shouldn't waste memory on storing the output.  Need to send it to /dev/null.
(define-syntax silently 
  (syntax-rules ()
    [(_ e ...)
     (let ((ret #f))
       (with-output-to-string
	 (lambda () (set! ret (begin e ...))))
       ret)]))


;; [2007.04.20] This is used to time a piece of code thats run from
;; separate places, and to add the cost of those executions together.
(define time-accum-buf (vector '()))
(define-syntax time-accum
  (syntax-rules ()
    [(_ e ...) 
     (let* ([start (cpu-time)]
	    [result (begin e ...)]
	    [end (cpu-time)])
       (vector-set! time-accum-buf 0 (cons (- end start) (vector-ref time-accum-buf 0)))
       result)]))
;; This discharges the saved runs and reports their total time:
(define (time-accum-report)
  (printf "Time-Accum: total cpu-time ~s over ~s runs: ~s\n" 
	  (apply + (vector-ref time-accum-buf 0)) 
	  (length  (vector-ref time-accum-buf 0))
	  (reverse (vector-ref time-accum-buf 0)))
  (vector-set! time-accum-buf 0 '()))

;; This is sometimes useful.  It's awkward to do manually.
(define-syntax parameterize-IFCHEZ
  (syntax-rules ()
    [(_ ([lhs rhs] ...) bod ...)
     ;; Don't worry about code duplication, only one branch will survive.
     (IFCHEZ (parameterize ([lhs rhs] ...) bod ...)
	     (let () bod ...))]))


;; [2007.05.16] Experimenting with this as an optimization.
;; Should move define-inlined here too...
(define-syntax map-inlined
  (syntax-rules ()
    [(_ f e)
     (let map-inlined-loop ([ls e])
       (if (null? ls) '()
	   (cons (f (car ls)) (map-inlined-loop (cdr ls)))))]))
(define-syntax map-tail-inlined
  (syntax-rules ()
    [(_ f e)
     (let map-inlined-loop ([ls e] [acc '()])
       (if (null? ls) (reverse! acc)
	   (map-inlined-loop (cdr ls) (cons (f (car ls)) acc))))]))



;; [2007.03.11] This isn't used yet much yet, but it may be used in
;; the future to disinclude the unit tests at compile time and
;; therefore speed up load time.  
;; 
;; Unfortunately, it just didn't speed things up much at all.
(define-syntax define-testing
  (lambda (syn)    
    (syntax-case syn ()
      [(_ x e) 
       ;; [2008.04.29] Register this name globally so that we can "pull" it...
       ;; It registers upon expansion, not evaluation.
       ;(reg:all-tester-names (cons (syntax->datum #'x) (reg:all-tester-names))) ;; TEMPTOGGLE
       #'(define x e)]
      ;; Disable testing:
      ;;[(_ x e) (define x '())]
      )))

(define-syntax debug-return-contract
  (syntax-rules ()
    [(_ pred fun) (debug-return-contract "<unknownFun>" pred fun)]
    [(_ name pred fun) 
     (IFDEBUG (lambda args 
		(let ([result (apply fun args)])
		  (if (pred result) result
		      (begin 
			(warning 'debug-return-contract "failed contract on return value, function: ~s\nvalue: ~s\nContinuation:\n"
				 'name result)
			(call/cc inspect)))))
	      fun)]))

) ;; End module.

;(require reg_macros) (test-reg_macros)
