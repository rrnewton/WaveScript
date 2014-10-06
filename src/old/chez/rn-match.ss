;; [2007.04.06] CODE DUPLICATION:
;;
;; This file is the same as "match.r5rs" also in the repository.
;; EXCEPT, I've hacked this one to use syntax-case for one purpose:
;; real ellipses.  The syntax-rules version had to settle for "_..."

;; Also I removed the "PATMATCH_" name mangling.

;; How far can we get doing match with syntax-rules?

;; Portability:
;; Chez -- ok
;; PLT -- ok
;; SCM -- ok (remember to run with -r 5)
;; gambit -- ok, load "~~/syntax-case.scm"
;; guile -- ok "(use-syntax (ice-9 syncase))"

;; stklos --

;; sisc   ??
;; gauche ?? 
;; kawa   ?? 

;; MIT -- works on some tests (eval doesn't, though)
;;        Breaks down on the first "->" test.
;;        Some weird thing wherein it tries to *apply* the results of the cata.
;; bigloo -- some kind of call-with-values error on the multiple value test
;; larceny -- gets a wrong number of arguments error on the same test as bigloo
;; gauche -- same wrong number of arguments

;;;; The NEXT project will be to make a version of match that's a
;;;; preprocessor for other match implementations.  It might do some
;;;; transformations like the following:
#|
(match FOO [(a ,_) 1] [(b ,_)  2] [(foo) 3])
(match FOO
  [(,hd . ,tl)
   (define (go-a)   (match tl [(,_) 1] [,oth (go-b)]))
   (define (go-b)   (match tl [(,_) 2] [,oth (go-foo)]))
   (define (go-foo) (match tl [() 3]))
   (case hd
     [(a)   (go-a)]
     [(b)   (go-b)]
     [(foo) (go-foo)]
     [else (error?)])])
|#


(module rn-match ((match match-help bind-dummy-vars bind-popped-vars exec-body 
			 build-list bind-cata ellipses-helper delay-values 
			 countup-vars countup-elements wrap-lambda-at-the-end 
			 vecref-helper vecref-helper2 rn-convert-pat
			 extend-backquote my-backquote
			 pop force-and-select
			 myforce
			 ;ellipsis?
			 )
		  ;; TEMP:
		  (rn-convert-pat ellipsis?)

		  (let-match match)
		  (match-lambda match match-lambda-helper)
		  
		  my-backquote
		  (extend-backquote my-backquote)
		  test-match 
		  tm2
		  ;ellipsis?
		  )
  (meta define ellipsis?
    (lambda (x)
      (and (identifier? x) (literal-identifier=? x #'(... ...)))))

;; For Chez, etc, we don't need to do anything special to delay with values.
 (define-syntax delay-values
  (syntax-rules ()
    ((_ e) (delay e))))

 (define simple-eval eval)

#;
  (define-syntax ellipsis?
    (syntax-rules ()
      [(_ x) (and (identifier? x) (literal-identifier=? x #'(... ...)))]))

  ;(include "generic/util/rn-match.r5rs")
  (include "ws/util/rn-match.r5rs")



;;;; THIS IS NOT R5RS:

;; TEMP: 
;; Using the quasiquote extension from match.ss
;; I need to rewrite this in a portable way or remove my dependency on it:

(define-syntax my-backquote
  (lambda (x)
    (define ellipsis?
      (lambda (x)
        (and (identifier? x) (literal-identifier=? x #'(... ...)))))
    (define-syntax with-values
      (syntax-rules ()
        ((_ P C) (call-with-values (lambda () P) C))))
    (define-syntax syntax-lambda
      (lambda (x)
        (syntax-case x ()
          ((_ (Pat ...) Body0 Body ...)
           (with-syntax (((X ...) (generate-temporaries #'(Pat ...))))
             #'(lambda (X ...)
                 (with-syntax ((Pat X) ...)
                   Body0 Body ...)))))))
    (define-syntax with-temp
      (syntax-rules ()
        ((_ V Body0 Body ...)
         (with-syntax (((V) (generate-temporaries '(x))))
           Body0 Body ...))))
    (define-syntax with-temps
      (syntax-rules ()
        ((_ (V ...) (Exp ...) Body0 Body ...)
         (with-syntax (((V ...) (generate-temporaries #'(Exp ...))))
           Body0 Body ...))))
    (define destruct
      (lambda (Orig x depth)
        (syntax-case x (quasiquote unquote unquote-splicing)
          ;; inner quasiquote
          ((quasiquote Exp)
           (with-values (destruct Orig #'Exp (add1 depth))
             (syntax-lambda (Builder Vars Exps)
               (if (null? #'Vars)
                   (values #''(quasiquote Exp) '() '())
                   (values #'(list 'quasiquote Builder) #'Vars #'Exps)))))
          ;; unquote
          ((unquote Exp)
           (zero? depth)
           (with-temp X
             (values #'X (list #'X) (list #'Exp))))
          ((unquote Exp)
           (with-values (destruct Orig #'Exp (sub1 depth))
             (syntax-lambda (Builder Vars Exps)
               (if (null? #'Vars)
                   (values #''(unquote Exp) '() '())
                   (values #'(list 'unquote Builder) #'Vars #'Exps)))))
          ;; splicing
          (((unquote-splicing Exp))
           (zero? depth)
           (with-temp X
             (values #'X (list #'X) (list #'Exp))))
          (((unquote-splicing Exp ...))
           (zero? depth)
           (with-temps (X ...) (Exp ...)
             (values #'(append X ...) #'(X ...) #'(Exp ...))))
          (((unquote-splicing Exp ...) . Rest)
           (zero? depth)
           (with-values (destruct Orig #'Rest depth)
             (syntax-lambda (Builder Vars Exps)
               (with-temps (X ...) (Exp ...)
                 (if (null? #'Vars)
                     (values #'(append X ... 'Rest)
                             #'(X ...) #'(Exp ...))
                     (values #'(append X ... Builder)
                             #'(X ... . Vars) #'(Exp ... . Exps)))))))
          ((unquote-splicing Exp ...)
           (with-values (destruct Orig #'(Exp ...) (sub1 depth))
             (syntax-lambda (Builder Vars Exps)
               (if (null? #'Vars)
                   (values #''(unquote-splicing Exp ...) '() '())
                   (values #'(cons 'unquote-splicing Builder)
                           #'Vars #'Exps)))))
          ;; dots
          (((unquote Exp) Dots)
           (and (zero? depth) (ellipsis? #'Dots))
           (with-temp X
             (values #'X (list #'X) (list #'Exp))))
          (((unquote Exp) Dots . Rest)
           (and (zero? depth) (ellipsis? #'Dots))
           (with-values (destruct Orig #'Rest depth)
             (syntax-lambda (RestBuilder RestVars RestExps)
               (with-syntax ((TailExp
                               (if (null? #'RestVars)
                                   #''Rest
                                   #'RestBuilder)))
                 (with-temp X
                   (values #'(append X TailExp)
                           (cons #'X #'RestVars)
                           (cons #'Exp #'RestExps)))))))
          ((Exp Dots . Rest)
           (and (zero? depth) (ellipsis? #'Dots))
           (with-values (destruct Orig #'Exp depth)
             (syntax-lambda (ExpBuilder (ExpVar ...) (ExpExp ...))
               (if (null? #'(ExpVar ...))
                   (syntax-error Orig "Bad ellipsis")
                   (with-values (destruct Orig #'Rest depth)
                     (syntax-lambda (RestBuilder RestVars RestExps)
                       (with-syntax ((TailExp
                                       (if (null? #'RestVars)
                                           #''Rest
                                           #'RestBuilder))
                                     (Orig Orig))
                         (values #'(let f ((ExpVar ExpVar) ...)
                                     (if (and (pair? ExpVar) ...)
                                         (cons
                                           (let ((ExpVar (car ExpVar)) ...)
                                             ExpBuilder)
                                           (f (cdr ExpVar) ...))
                                         (if (and (null? ExpVar) ...)
                                             TailExp
                                             (error 'unquote
                                               "Mismatched lists in ~s"
                                               Orig))))
                                 (append #'(ExpVar ...) #'RestVars)
                                 (append #'(ExpExp ...) #'RestExps)))))))))
	  ;; Vectors
	  (#(X ...)
	   (with-values (destruct Orig #'(X ...) depth)
	     (syntax-lambda (LsBuilder LsVars LsExps)
	       (values #'(list->vector LsBuilder) #'LsVars #'LsExps))))
          ;; random stuff
          ((Hd . Tl)
           (with-values (destruct Orig #'Hd depth)
             (syntax-lambda (HdBuilder HdVars HdExps)
               (with-values (destruct Orig #'Tl depth)
                 (syntax-lambda (TlBuilder TlVars TlExps)
                   (with-syntax ((Hd (if (null? #'HdVars)
                                         #''Hd
                                         #'HdBuilder))
                                 (Tl (if (null? #'TlVars)
                                         #''Tl
                                         #'TlBuilder)))
                     (values #'(cons Hd Tl)
                             (append #'HdVars #'TlVars)
                             (append #'HdExps #'TlExps))))))))
          (OtherThing
            (values #''OtherThing '() '())))))
    ;; macro begins
    (syntax-case x ()
      ((_ Datum)
       (with-values (destruct #'(quasiquote Datum) #'Datum 0)
         (syntax-lambda (Builder (Var ...) (Exp ...))
           (if (null? #'(Var ...))
               #''Datum
               #'(let ((Var Exp) ...)
                   Builder))))))))

(define-syntax extend-backquote
  (lambda (x)
    (syntax-case x ()
      ((_ Template Exp ...)
       (with-syntax ((quasiquote
                       (datum->syntax-object #'Template 'FLUB)))
         #'(let-syntax ((quasiquote
                          (lambda (x)
                            (syntax-case x ()
                              ((_ Foo) #'(my-backquote Foo))))))
             
	     (let () Exp ...)
	     ))))))



) ;; End module




#!eof




  (collect 4)(define val '(foo 1 2 (bar 3 4 5)))
  (time (rep 10000000 (match val [(foo ,x ,y) 'no]
     [(bar ,[x] ,[y] ,[z]) `(bar ,x ,y ,z)] [(foo ,[x] ,[y] ,[z]) `(foo ,x ,y ,z)] [,_ 0])))


  (collect 4)(define val #(foo 1 2 #(bar 3 4 5)))
  (time (rep 10000000 (match val [#(foo ,x ,y) 'no]
     [#(bar ,[x] ,[y] ,[z]) `#(bar ,x ,y ,z)] [#(foo ,[x] ,[y] ,[z]) `#(foo ,x ,y ,z)] [,_ 0])))






(match '(timer 3.0)
  [(,prim ,[rand*] ...)
   (guard (wavescript-primitive? prim))
   9999]
  [,oth 78])


(define (print-var-types exp max-depth . p)
  (IFCHEZ (import rn-match) (void))
  (let ([port (if (null? p) (current-output-port) (car p))])
    
    (trace-define (get-var-types exp)
)

   
    (let loop ([x (get-var-types exp)] [depth 0] [indent " "])
      (if (= depth max-depth) (void)
	  (match x
	    [() (void)]
	    [(type ,v ,t ,subvars)
	     (unless (eq? v '___VIRTQUEUE___) 	 ;; <-- HACK: 
	       (fprintf port "~a~a :: " indent v)
	       (print-type t port) (newline port))
	     (loop subvars (fx+ 1 depth) (** indent "  "))]
	    [,ls (guard (list? ls))
		 (for-each (lambda (x) (loop x depth indent))
		   ls)]
	    [,other (error 'print-var-types "bad result from get-var-types: ~a" other)])))
      ))




      (match '(timer 3.0)

       [(,lang '(program ,[body] ,meta ... ))
	 (append body `((type BASE ,(last meta) ())))]

       [,c (guard (simple-constant? c)) '()]
       [,var (guard (symbol? var))  `()]       
       [(quote ,c)       '()]
       [(assert-type ,t ,[e]) e]
       [(set! ,v ,[e]) e]
       [(begin ,[e*] ...) (apply append e*)]
       [(for (,i ,[s] ,[e]) ,[bodls]) (cons `[type ,i Int ()] (append s e bodls))]
       [(while ,[tstls] ,[bodls]) (append tstls bodls)]

       [(if ,[t] ,[c] ,[a]) (append t c a)]
       [(tuple ,[args] ...) (apply append args)]
       [(tupref ,n ,m ,[x]) x]
       [(unionN ,[args] ...) (apply append args)]

       [(,let ([,id* ,t* ,[rhs*]] ...) ,[bod]) 
	(guard (memq let '(let letrec lazy-letrec)))
	(append (apply append 
		       (map (lambda (id t rhsls)
			      `([type ,id ,t ,rhsls]))
			 id* t* rhs*))
		bod)]
       [(lambda ,v* ,t* ,[bodls])   bodls]
       [(,app ,[rat] ,[rand*] ...) (guard (memq app '(app construct-data)))
	(apply append rat rand*)]
       [(,prim ,[rand*] ...)
	 (guard (wavescript-primitive? prim))
	 (apply append rand*)]
	[,other (error 'print-var-types "bad expression: ~a" other)])


(begin (print-gensym #f)(print-graph #f)(expand '(match 3 [3 4])))
(begin (optimize-level 2)(print-gensym£h #f)(print-graph #f)(expand/optimize '(match 3 [3 4])))




  (collect 4)
  (time (rep 50000 ))

(define val '(foo 1 2 (bar 3 4 5)))
(match val [('foo x y) 'no] 
       [('bar x y z) `(bar ,(f x) ,(f y) ,(f z))]
       [('foo x y z) `(foo ,(f x) ,(f y) ,(f z))]
       [_ 0])



;(import rn-match)(match '() [(,z ...) (vector x y z)]))

