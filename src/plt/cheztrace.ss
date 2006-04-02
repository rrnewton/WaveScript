;; [2005.11.13 ]  Having a go at implementing these in PLT:
;; DOESNT WORK YET.

(module cheztrace mzscheme
;  (require (lib "errortrace.ss" "errortrace")
;           (lib "stacktrace.ss" "errortrace"))
  (require (lib "trace.ss"))
  
  (provide trace-lambda trace-define trace-let inspect break)

  ;; Just stubs.
  (define (inspect x) (void))
  (define (break x) x)
  
  
  #;
  (define-syntax trace-lambda
    (lambda (x)
      (syntax-case x ()
        [(_ params  bods ...) 
         #`(lambda params 
             #,(annotate #'(begin bods ...) #f))])))

  (define-syntax trace-lambda
    (lambda (x)
      (syntax-case x ()
        [(_ name (params ...) bods ...)
         (identifier? #'name)
         #'(let ((name (lambda (params ...) bods ...)))
             (trace name)
             name)])))
#|      
  (define-syntax trace-define
    (lambda (x)
      (syntax-case x ()
        [(_ (id v ...) e) #'(begin (define (id v ...) e) (trace id))]
        [(_ x e) (identifier? #'x) #'(begin (define x e) (trace x))])))
  
  (define-syntax trace-let
    (syntax-rules ()
      [(_ n ([l* r*] ...) bods ...)
       (letrec ((n (lambda (l* ...) bods ...)))
         (trace n)
         (n r* ...))]))
|#

;; [2006.03.23] For now killing these ^^^, not sure how much they worked.

;; These simply don't do anything under PLT for the moment.
(define-syntax trace-define (syntax-rules () [(_ e ...) (define e ...)]))
(define-syntax trace-let (syntax-rules () [(_ e ...) (let e ...)]))
  

)

;(require cheztrace)
;(define f (trace-lambda (x) x (if (odd? x) (error 'foo "") (add1 x))))
;(f 40)
;(f 39)

;(trace-let loop ((n 10))
;           (if (> n 0) (loop (sub1 n))))
