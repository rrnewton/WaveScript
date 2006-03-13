
;; NOTE: This is used ONLY, by constants.ss.  

#cs ;; Case Sensitivity
(module plt_constants mzscheme
	(require (lib "include.ss")
		 "../generic/constants.ss")

	(provide 
	 IFCHEZ
	 IF_GRAPHICS
	 reg:define-struct
         reg:struct?
	 reg:struct->list

	 chezimports chezprovide
	 
	 (all-from "../generic/plt_constants.ss"))

	
  ;; Pre-processor macro for switching between Chez/PLT versions.
  (define-syntax IFCHEZ (syntax-rules () [(_ chez plt) plt]))

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
	  (cons (string-copy name)
		(let loop ([i 0])
		  (if (= i stop)
		      '()
		      (cons (access s i) (loop (add1 i))))))
	  ))))

)

;(require constants)
