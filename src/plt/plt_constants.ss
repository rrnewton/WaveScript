
;; NOTE: This is used ONLY, by constants.ss.  

#cs ;; Case Sensitivity
(module plt_constants mzscheme
	(require (lib "include.ss")
		 )

	(provide 

	 IFCHEZ
	 IF_GRAPHICS 
	 IF_THREADS
	 cond-expand
	 reg:define-struct
         reg:struct?
	 reg:struct->list
         reg:list->struct	
	 )
	
  ;; Pre-processor macro for switching between Chez/PLT versions.
  (define-syntax IFCHEZ (syntax-rules () [(_ chez plt) plt]))
  
  ;; [2007.12.27] Considering switching over to a cond-expand system.
  (define-syntax cond-expand
    (lambda (syn)
      (syntax-case syn (and or not else 
			    chez plt larceny graphics threads)
      ;; Standard clauses:
      ((cond-expand) (raise-syntax-error #f "Unfulfilled cond-expand" #'syn))
      ((cond-expand (else body ...))  #'(begin body ...))
      ((cond-expand ((and) body ...) more-clauses ...) #'(begin body ...))
      ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
       #'(cond-expand (req1 (cond-expand ((and req2 ...) body ...) more-clauses ...)) more-clauses ...))
      ((cond-expand ((or) body ...) more-clauses ...) #'(cond-expand more-clauses ...))
      ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
       #'(cond-expand (req1 (begin body ...)) (else (cond-expand ((or req2 ...) body ...) more-clauses ...))))
      ((cond-expand ((not req) body ...) more-clauses ...)
       #'(cond-expand (req (cond-expand more-clauses ...)) (else body ...)))

      ;; Enabled features (or potentially enabled):
      ((cond-expand (plt body ...) more-clauses ...) #'(begin body ...))
      ;; 'threads' and 'graphics' are disabled for now under plt.
      
      ;; Otherwise, it's disabled:
      ((cond-expand (feature-id body ...) more-clauses ...) #'(cond-expand more-clauses ...)))))


  ;; [2005.11.04] This is lame, but the only way I know of to check for MrED is
  ;; to try to require it -- if we get an error, it's not there.
#;
   (define-syntax IF_GRAPHICS
    (lambda (x)
      (let ((flag (let/ec esc
               (parameterize ([error-display-handler (lambda (ob s) (void))]
                              [error-escape-handler (lambda args (esc #f))])
                 (eval '(require (lib "mred.ss" "mred")))
                 (esc #t)))))
        (syntax-case  x ()
          [(_ E1 E2) (if flag #'E1 #'E2)]
          [(_ E1) (if flag #'E1)]))))
   ;; [2005.11.04] I was having trouble with that.  When compiling from
   ;; command line I'd get an error.  Let's just set it manually:
   (define-syntax IF_GRAPHICS
     (syntax-rules ()
       [(_ t f) f]
       [(_ t)   (void)]))

   ;; [2006.07.28] Not supporting THREADS under PLT currently (but this would be easy).
   (define-syntax IF_THREADS
     (syntax-rules ()
       [(_ t f) f]
       [(_ t)   (void)]))


   (define-syntax reg:define-struct
     (syntax-rules ()
       [(_ (sname field ...))
	(define-struct sname (field ...) 
	  ;;(current-inspector))])) 
	  ;;(make-inspector))]))
	  #f)]))
  (define reg:struct? struct?)
  (define (reg:struct->list s)
    (let-values ([(strty _) (struct-info s)])
      (let-values ([(name initf autof access mutat immutlst super skipped?) (struct-type-info strty)])
	(let ([stop (+ initf autof)])
	  (cons (string-copy (format "~a" name))
		(let loop ([i 0])
		  (if (= i stop)
		      '()
		      (cons (access s i) (loop (add1 i))))))
	  ))))
  (define (reg:list->struct . args)
    (error reg:list->struct "Not implemented in PLT yet!!"))

)

;(require constants)
