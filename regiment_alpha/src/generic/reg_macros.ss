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

(module reg_macros mzscheme 
  
  (require 
   "../plt/iu-match.ss"
   "../generic/constants.ss"
   "../plt/chez_compat.ss"
   )

  (provide
      for grep rep
      let-match 
      mvlet
      match-lambda
      ++ ^ ;; Exponentiation
      define-id-syntax
      rec 
      ;;reg:define-struct ;; Moved to constants.ss 
      apply-ordered
   )
  
  ;; These provide extra information for Chez:
  (chezprovide (match-lambda match-lambda-helper))
  
  (chezimports )

  ;===============================================================================

  (IFCHEZ
   ;; multiple-value let
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
		     (domvlet #'(((id ...) expr) ...) '() '() #'(form ...))])))
   ;; In PLT this is just the same as let-values.
   (define-syntax mvlet
     (syntax-rules ()
       [(mvlet stuff ...) (let-values stuff ...)])))


  (define-syntax rec
    (syntax-rules ()
      ((_ x e) (letrec ((x e)) x))))


;; Repeatedly execute an expression some number of times:
(define-syntax rep
  (syntax-rules ()
    [(_ reps exp ...)
     (let loop ([n reps])
       (if (< n 1)
           (void)
           (begin exp ...
                  (loop (sub1 n)))))]))

;; [2005.10.16]  I hate 'do' syntax, for loops are much cleaner.
;; For now we only allow fixnum indices:
(define-syntax for
  (syntax-rules (= to)
    [(_ v = start to end expr ...)
     (let ((s start)
	   (e end))
       (do ([v s (fx+ v 1)])
	   ((fx> v e))
	 expr ...))]))

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
      [(_ () (V ...) E ...)
       #'(lambda (V ...) E ...)]
      [(_ (P1 P ...) (V ...) E ...)
       #'(match-lambda-helper (P ...) (V ... tmp) (let-match ((P1 tmp)) E ...))])))
(define-syntax match-lambda 
  (lambda (x)
    (syntax-case x (unquote)
      [(_ (Pat ...) Expr ...)
       ;(printf "Woot: ~a\n" #'(Pat ...))
       #'(match-lambda-helper (Pat ...) () Expr ...)
       ])))
;(expand '((match-lambda (x y) #t) 'x 'y))
;(expand '(match-lambda (,x ,y) #t))
;((match-lambda ((,x ,y) #(,z) ,w) (list x z w)) `(a b) #(3) 4)


;; This allows pattern matching in the LHSs of let's.
(define-syntax let-match
  (lambda (x)
    (syntax-case x (unquote)
      [(_ () Body ...)
       #'(begin Body ...)]
      [(_ ([Pat Exp] Rest ...) Body ...)
       #'(match Exp
	   [Pat (let-match (Rest ...) Body ...)]
	   [,other (error 'let-match "unmatched object: ~s" other)]
	   )])))
;(expand '(let-match () 3))
;(expand '(let-match ([,x 3]) x))
;(let-match ([(,x ,y ...) (list 3 4 5 6)] [,z 99]) (list y z))


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
	 (disp "Woot:" (map identifier? #'(x ...)))
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
				    (lambda (_) (datum->syntax-object #'appord (gensym "tmp")))
				  (syntax-object->datum #'(x ...)))))
	 #'(let* ((tmp x) ...) (f tmp ...)))])))

;; '++' is a string-append shorthand
#;(define-syntax ++
  (lambda (x)
    (syntax-case x ()
		 [id (identifier? #'id) #'string-append]
		 [(_ E ...) #'(string-append E ...)])))
(define ++ string-append)  ;; Trust the inliner. [2006.03.02]

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


) ;; End module.

;(require reg_macros)
