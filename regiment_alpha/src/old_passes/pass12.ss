;; A BIT OUT OF DATE RIGHT NOW

;;; Pass 12: introduce-cps
;===============================================================================
;; This is Dan's new and improved cps algorithm:

;; If you are writing cases for this CPS algorithm... you can think
;; of possible-letk as what allows you to refer to 'k' by name...
;;


;;; TRY TO THINK OF THE TRIVIAL CASES WITH THIS ALGORITHM...
;;; say somethings a constant, when it comes around it'll just feed
;;; the constant into the context..

(define compare-cps-algorithms
  (lambda ()
    (define start)
    (define timea)
    (define timeb)
    (set! start (statistics))
    (printf "Running Dan's new algorithm:~n")
    (test-all)
    (set! timea (sstats-difference (statistics) start))
    (load "./src/passes/pass10_old.ss") ;;Old cps algoritm
    (set! start (statistics))
    (printf "Running the old algorithm:~n")
    (test-all)
    (set! timeb (sstats-difference (statistics) start))
    (printf ";----------------------------~n")
    (printf "; OK HERE ARE THE RESULTS:~n~n")
    (printf "Dan's new algorithm:~n")
    (sstats-print timea)(newline)
    (printf "The old essentials 2 algorithm:~n")
    (sstats-print timeb)(newline)
    (define-top-level-value 'newalg timea)
    (define-top-level-value 'oldalg timeb)
    ))

(define introduce-cps
  (let ()
    ;---------------------------------------
    (define generate-k
      (lambda ()
        (unique-name 'k)))  ;; This could just return 'k
    ;---------------------------------------
    ;;Continuation Lambda:
    (define CLAM
      (lambda (E)
        (let ((formal (unique-name 'v)))
          (let ((body (E formal)))
            `(lambda (,formal) ,body)))))
    ;---------------------------------------
    (define possible-letk
      (lambda (cont-exp abs-body)
        (let ([k (generate-k)])
          (let ([body (abs-body k)])
            `(let ((,k ,cont-exp))
               ,body)))))
    ;---------------------------------------
    (define cps*
      (lambda (N* E)
        (cond
          ((null? N*) (E '()))
          (else (cps (car N*)
                     (lambda (a)
                       (cps* (cdr N*)
                             (lambda (d) (E (cons a d))))))))))
    ;---------------------------------------
    (define cps
      (lambda (t E)
        (match t
          [,x (guard (symbol? x)) (E x)]
          [(quote ,c) (E `(quote ,c))]
          [(toplvl-varref ,var)
           (E `(toplvl-varref ,var))]
          [(toplvl-varassign! ,var ,rhs)
           (possible-letk (CLAM E)
                          (lambda (k)
                            (cps rhs
                                 (lambda (v)
                                   `(toplvl-varassign! ,var ,v)))))]
          [(if ,test-exp ,true-exp ,false-exp)
           (possible-letk (CLAM E)
                          (lambda (k)
                            (cps test-exp
                                 (lambda (v)
                                   `(if ,v
                                        ,(cps true-exp (lambda (v) `(,k ,v)))
                                        ,(cps false-exp (lambda (v) `(,k ,v))))))))]
          [(begin ,expr) (cps expr E)]
          [(begin ,expr ,expr* ...)
           (cps expr
                (lambda (v)
                  (if (symbol? v)
                      (cps `(begin ,expr* ...) E)
                      (make-begin (list v (cps `(begin ,expr* ...) E)))
                      )))]
          [(lambda (,x ...) ,M)
           (let ([k (generate-k)])
             (E `(lambda (,x ... ,k)
                   ,(cps M (lambda (v) `(,k ,v))))))]
          [(let ((,x ,N) ...) ,body)
           (possible-letk (CLAM E)
                          (lambda (k)
                            (cps* `(,N ...)
                                  (lambda (w*)
                                    `(let ,(map list `(,x ...) w*)
                                       ,(cps body (lambda (v) `(,k ,v))))))))]
          [(letrec ((,xs (lambda (,idss ...) ,bodys)) ...) ,letrec-body)
           (possible-letk (CLAM E)
                          (lambda (k)
                            `(letrec
                               ,(map list `(,xs ...)
                                     (map (lambda (ids body)
                                            (let ([k (generate-k)])
                                              `(lambda (,ids ... ,k)
                                                 ,(cps body (lambda (v) `(,k ,v))))))
                                          idss bodys))
                               ,(cps letrec-body (lambda (v) `(,k ,v))))))]
          
          ;-----
          [(call/cc (lambda (,id) ,body))
           (let ([v (unique-name 'v)])
             (possible-letk (CLAM E)
                            (lambda (k)
                              `(let ([,id (lambda (,v _) (,k ,v))])
                                 ,(cps body (lambda (v) `(,k ,v)))))))]
          [(call/cc ,fun)
           (let ([v (unique-name 'v)])
             (possible-letk (CLAM E)
                            (lambda (k)
                              (cps fun
                                   (lambda (f)
                                     `(,f (lambda (,v _) (,k ,v)) ,k)
                                     )))))]
          ;-----
          [(,prim ,N ...)
           (guard (extended-scheme-primitive? prim))
           (cps* `(,N ...)
                 (lambda (w*)
                   (E `(,prim ,@w*))))]
          [(,M ,NS ...)
           (cps M
                (lambda (v)
                  (cps* `(,NS ...)
                        (lambda (w*)
                          `(,v ,@w* ,(CLAM E))))))])))
    ;---------------------------------------
    (define output-language-definition
      '(begin))
    ;---------------------------------------
    (lambda (prog)
      (match prog
        [(let () ,input-language-definition ,expr)
         
         `(let ()
            ,output-language-definition
            ,(cps expr (lambda (v) v))
            )]))))


;	 (let ([x (unique-name 'x)])
;	   `(let ()
;	      ,output-language-definition
;	      (let ([k (lambda (,x) ,x)])
;		,(cps expr (lambda (v) `(k ,v))))))

;; Dan thinks that there should be a toplevel k:
;	    ,(let ([k (generate-k)]
;		   [x (unique-name 'x)])
;	      `(let ([,k (lambda (,x) ,x)])
;		 ,(cps expr (lambda (v) `(,k ,v)))))

;===============================================================================


#!eof

;;Wimpy versions:


(define CLAM
  (lambda (E)
    (let ([formal (unique-name 'v)])
      (let ((body (E formal)))
        `(lambda (,formal) ,body)))))
;---------------------------------------
(define possible-letk
  (lambda (cont-exp body)
    `(let ((k ,cont-exp))
       ,body)))



