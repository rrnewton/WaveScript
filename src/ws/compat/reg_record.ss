

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