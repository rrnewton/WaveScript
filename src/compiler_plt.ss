;; Ryan Newton
;; Started 2004.03.26
;; Here is the main file for my compiler.
;;======================================================================

;(module compiler_plt mzscheme

(require (lib "include.ss")
         (all-except "plt/helpers.ss" id)
         ;	 "plt/language-mechanism.ss"
         
         (all-except "plt/pass00_verify-regiment.ss" these-tests test-this)
         (all-except "plt/pass01_rename-var.ss" these-tests test-this)
       
         
         "plt/pass02_remove-unquoted-constant.ss"
         
         "plt/pass03_remove-complex-constant.ss"
         "plt/pass04_uncover-free.ss"
         "plt/pass05_lift-letrec.ss"
         "plt/pass06_lift-letrec-body.ss"
         "plt/pass07_remove-complex-opera.ss"
         
         (all-except "plt/pass08_verify-core.ss" these-tests test-this)
         (all-except "plt/pass10_deglobalize.ss" these-tests test-this)
        ;          "plt/pass09_separate-graph.ss"
         
         )

(disp "UNION" union (union '(a b c) '(a d c)))

'(define program 
   (lambda args (car (reverse args))))

'(define base-language 
   (lambda args (eval (car (reverse args)))))
;(define base-language 
;  (lambda args
;    (for-each eval args)))

;(disp "FOOB DONE REQS")
;(require "plt/language-mechanism.ss")
;(disp "DOEN LANG")

;  (require "plt/demo_display.ss")

;; Get those module bound identifiers out in the open!
(load "plt/tests.ss") ;(require tests)
;(define tests tests)

;(define test00 test00)
;(define test01 test01)
;(define test07 test07)

;(define tests00 tests00)
;(define tests01 tests01)
;(define tests07 tests07)

(disp "BOUT TO LOAD COMPLIRE" )
(load/use-compiled "compiler.ss")

(disp "BOUT TO LOAD DRIVER" pass-names)

(load/use-compiled "plt/driver.ss")
;; Can't get langs to work.  Just abandon evaluation:
(game-eval (lambda args 'unspecified))
(host-eval (lambda args 'unspecified))

;(require "plt/language-mechanism.ss") ;; This blows up if we try to require it up top!
;(load/use-compiled "generic/lang00.ss")
;(load/use-compiled "generic/lang05.ss")

(require (lib "trace.ss"))
(trace  explode-primitive process-expr process-letrec)

(define (test-this)
  (parameterize ((tracer #t))
    (test-one
     '(letrec ((a (anchor-at '(30 40)))
               (r (circle-at 50 a))
               (f (lambda (tot next)
                    (cons (+ (car tot) (sense next))
                          (+ (cdr tot) 1))))
               (g (lambda (tot) (/ (car tot) (cdr tot))))
               (avg (smap g (rfold f (cons 0 0) r))))
        avg))))

(define (g) (test-this))
(define (b) (t '(circle 1 2)))