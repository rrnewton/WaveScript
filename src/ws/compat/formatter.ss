  
  ;; [2008.04.25] Grabbed the formatter from ikarus, it's GPL 3:
  (define formatter
    (lambda (who p fmt args)
      ;;; first check
      (let f ([i 0] [args args])
        (cond
          [(fx=? i (string-length fmt))
           (unless (null? args) 
             (die who 
               (format 
                 "extra arguments given for format string \x2036;~a\x2033;"
                 fmt)))]
          [else
           (let ([c (string-ref fmt i)])
             (cond
               [(eqv? c #\~)
                (let ([i (fxadd1 i)])
                  (when (fx=? i (string-length fmt))
                    (die who "invalid ~ at end of format string" fmt))
                  (let ([c (string-ref fmt i)])
                   (cond
                     [(memv c '(#\~ #\%)) (f (fxadd1 i) args)]
                     [(memv c '(#\a #\s))
                      (when (null? args)
                        (die who "insufficient arguments"))
                      (f (fxadd1 i) (cdr args))]
                     [(memv c '(#\b #\o #\x #\d))
                      (when (null? args)
                        (die who "insufficient arguments"))
                      (let ([a (car args)])
                        (cond
			 #;
			 [(or (fixnum? a) (bignum? a) (ratnum?  a)) 
			  (void)]
                          [(flonum? a)
                           (unless (eqv? c #\d) 
                             (die who 
                               (format "flonums cannot be printed with ~~~a" c)))]
			  ;; RRN: Probably less efficient, but no "bignum?" in R6RS:
			  [(number? a) (void)]
                          [else 
                           (die who "not a number" a)]))
                      (f (fxadd1 i) (cdr args))]
                     [else
                      (die who "invalid sequence character after ~" c)])))]
               [else (f (fxadd1 i) args)]))]))
      ;;; then format
      (let f ([i 0] [args args])
        (unless (fx=? i (string-length fmt))
          (let ([c (string-ref fmt i)])
            (cond
              [(eqv? c #\~)
               (let ([i (fxadd1 i)])
                 (let ([c (string-ref fmt i)])
                  (cond
                    [(eqv? c #\~) 
                     (write-char #\~ p)
                     (f (fxadd1 i) args)]
                    [(eqv? c #\%) 
                     (write-char #\newline p)
                     (f (fxadd1 i) args)]
                    [(eqv? c #\a)
                     (display (car args) p)
                     (f (fxadd1 i) (cdr args))]
                    [(eqv? c #\s)
                     (write (car args) p)
                     (f (fxadd1 i) (cdr args))]
                    [(assv c '([#\b . 2] [#\o . 8] [#\x . 16] [#\d . 10]))
                     =>
                     (lambda (x)
                       (let ([a (car args)])
                         (cond
			  #;
			  [(or (fixnum? a) (bignum? a) (ratnum? a))
			   (display (number->string a (cdr x)) p)]
                           [(flonum? a)
                            (display (number->string a) p)]
			   ;; RRN: Probably less efficient, but no "bignum?" in R6RS:
			   [(number? a) (display (number->string a (cdr x)) p)]
                           [else (die who "BUG: not a number" a)]))
                       (f (fxadd1 i) (cdr args)))]
                    [else (die who "BUG" c)])))]
              [else 
               (write-char c p)
               (f (fxadd1 i) args)]))))
      ;;; then flush
      (flush-output-port p)))

  (define format
    (lambda (fmt . args)
      (unless (string? fmt)
        (die 'format "not a string" fmt))
      (let-values ([(p e) (open-string-output-port)])
        (formatter 'format p fmt args)
        (e))))
  
  (define fprintf
    (lambda (p fmt . args)
      ;; RRN:
      ;;(assert-open-textual-output-port p 'fprintf)
      (unless (and (textual-port? p) (output-port? p))
	(die 'fprintf "not a textual output port" p))
      (unless (string? fmt)
        (die 'fprintf "not a string" fmt))
      (formatter 'fprintf p fmt args)))
   
  (define printf 
    (lambda (fmt . args)
      (unless (string? fmt)
        (die 'printf "not a string" fmt))
      (formatter 'printf (current-output-port) fmt args)))

