
;; Shared compatibility layer pieces.

;; This defines the regiment records to be R6RS records:
(define-syntax reg:define-struct
  (lambda (x)      
    (define (symappend . syms) 
      (define (coerce x) (if (string? x) x (symbol->string x)))
      (string->symbol (apply string-append (map coerce syms))))
    (syntax-case x (fields mutable)
		 [(_ (name fld* ...))		  
		  (let* ([tosyntax (lambda (s) (datum->syntax #'name s))]
			 [thename (syntax->datum #'name)]
			 [syms (syntax->datum #'(fld* ...))]
			 [accessors (map (lambda (s) (symappend thename "-" s)) syms)]
			 [mutators  (map (lambda (s) (symappend "set-" thename "-" s "!")) syms)])
		    (let* ([acc* (map tosyntax accessors)]
			   [mut* (map tosyntax mutators)]
			   [mutflds (map (lambda (fld acc mut) #`(mutable #,fld #,acc #,mut))
				      #'(fld* ...) acc* mut*)])
		      ;(printf "MUTABLE FIELDS: ~a\n" (syntax->datum mutflds))
		      #`(define-record-type name (fields . #,mutflds))
		      ))])))

(define reg:struct? record?)

(define-syntax fluid-let
    (lambda (x)
      (syntax-case x ()
		   ((_ () e1 e2 ...) #'(let () e1 e2 ...))
		   ((_ ((x v) ...) e1 e2 ...)
		    (for-all identifier? #'(x ...))
		    (with-syntax (((y ...) (generate-temporaries #'(x ...))))
		      #'(let ((y v) ...)
			  (let ((swap (lambda ()
					(let ((t x)) (set! x y) (set! y t)) ...)))
			    (dynamic-wind swap (lambda () e1 e2 ...) swap))))))))

(define merge
  (lambda (pred? l1 l2)
    (cond
     ((null? l1) l2)
     ((null? l2) l1)
     ((pred? (car l2) (car l1))
      (cons (car l2) (merge pred? l1 (cdr l2))))
     (else (cons (car l1) (merge pred? (cdr l1) l2))))))
(define merge! merge)


#;
(define-syntax datum
    (syntax-rules ()
      [(_ t) (syntax->datum (syntax t))]))
  
(define syntax->list
    (lambda (ls)
      (syntax-case ls ()
	[() '()]
	[(x . r) (cons #'x (syntax->list #'r))])))


#;
  (define-syntax identifier-syntax
   (lambda (x)
     (syntax-case x ()
      [(_ e)
       #'(lambda (x)
           (syntax-case x ()
             [id  (identifier? #'id)  #'e]
             [(id x (... ...))  (identifier? #'id)
              #'(e x (... ...))]))])))


  (define (simple-eval xp) (eval xp '(rnrs (6))))
