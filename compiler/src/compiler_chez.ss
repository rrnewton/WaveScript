;; TEMP
(define current_interpreter 'chezscheme)


(printf "Loading compiler in chezscheme..~n")

(include "chez/match.ss")

;(include "generic/constants.ss")

;; [2004.06.04] MOVED THIS DEFINIITON to helpers.ss
;; Uncomment this to remove debugging code and possibly make the
;; system run faster.
;;(define-syntax DEBUGMODE (syntax-rules () [(_ expr ...) (void)]))
;;(define-syntax DEBUGMODE (syntax-rules () [(_ expr ...) (begin expr ...)]))

;; [2004.06.28] Moving this here, hope that works:
(load "../depends/slib/chez.init")
(require 'tsort) ;; for the simulator: 

(load "generic/constants.ss")

;; This in turn includes "../generic/helpers.ss" so we gotta load it from its dir.
(cd "chez") (include "helpers.ss") (cd "..")

(include "generic/language-mechanism.ss")
(include "generic/lang00.ss")
(include "generic/lang05.ss")
(include "generic/lang10.ss") ;; deglobalize

(include "generic/pass00_verify-regiment.ss")
(include "generic/pass01_eta-primitives.ss")
(include "generic/pass02_rename-vars.ss")
(include "generic/pass03_remove-unquoted-constant.ss")
(include "generic/pass04_reduce-primitives.ss")
(include "generic/pass05_remove-complex-constant.ss")
(include "generic/pass06_uncover-free.ss")
(include "generic/pass07_lift-letrec.ss")
(include "generic/pass08_lift-letrec-body.ss")
(include "generic/pass09_remove-complex-opera.ss")
(include "generic/pass10_verify-core.ss")
(include "generic/pass11_classify-names.ss")
;(include "generic/pass09_separate-graph")
(include "generic/pass12_annotate-heartbeats.ss")
(include "generic/pass13_deglobalize.ss")
(include "generic/pass14_cleanup-token-machine.ss")
;(include "generic/pass15_verify-token-machine.ss")

;(load "../depends/slib/chez.init")
;(require 'tsort) ;; for the simulator: 

;; Basic parallel computation (engines):
(if (top-level-bound? 'SWL-ACTIVE)
    (load "chez/swl_flat_threads.ss")
    (load "chez/flat_threads.ss"))

;; LAME:
;(if (top-level-bound? 'SWL-ACTIVE) (eval '(import flat_threads)))

;; Load this before the simulator.
(if (top-level-bound? 'SWL-ACTIVE)
    (begin
      (load "chez/basic_graphics.ss")
      (load "chez/graphics_stub.ss")
      (eval '(import basic_graphics))
      (eval '(import graphics_stub)))
    (eval '(begin (define draw-mark (lambda args (void)))
		  (define rgb (lambda args (void))))))

;; Basic simulator for the nodal language:
;(include "chez/simulator_nought.ss")
(load "chez/simulator_nought.ss")

;; If we're in SWL then load the GRAPHICS portion:
(when (top-level-bound? 'SWL-ACTIVE)
      (load "chez/demo_display.ss")
      (load "chez/simulator_nought_graphics.ss"))

;(trace  explode-primitive process-expr process-letrec)

(include "compiler.ss")

;; Driver depends on 'pass-names being defined.
(include "generic/driver.ss")
  (game-eval (lambda args 'unspecified))
  (host-eval (lambda args 'unspecified))
(include "generic/tests_noclosure.ss")
(include "generic/tests.ss")

;; Load the repl which depends on the whole compiler and simulator.
(include "generic/repl.ss")

(define text-repl  (repl-builder void void run-compiler run-simulation-stream))
(define precomp-repl (repl-builder 
		      void  ;; Startup
		      void  ;; Cleanse
		      (lambda (x) x) ;; Compiler
		      run-simulation-stream)) ;; Runner

;; ???????
(define precomp-graphical-repl (repl-builder 
		      void  ;; Startup
		      void  ;; Cleanse
		      (lambda (x) x) ;; Compiler
		      run-simulation-stream))


(if (top-level-bound? 'SWL-ACTIVE)
    (begin
      (eval '(import basic_graphics))
      (eval '(import graphics_stub))
      (load "chez/simulator_nought_graphics.ss")

      (let ([grepl-init (lambda () 
			  (init-world)
			  (init-graphics))]
	    [grepl-cleanse 
		      ;; Inbetween evaluations, reset colors.
		      (lambda ()
			(for-each
			 (lambda (simob)
			   (if (simobject-gobj simob)
			       (set-fill-color! (simobject-gobj simob) 
						Starting-Node-Color)))
			 all-objs)
			(cleanse-world))])
	(define-top-level-value 'graphical-repl
	  (repl-builder grepl-init 
			grepl-cleanse
			run-compiler
			graphical-simulation))
	(define-top-level-value 'precomp-graphical-repl
	  (repl-builder grepl-init 
			grepl-cleanse
			(lambda (x) x)
			graphical-simulation)))

      (define-top-level-value 'grepl graphical-repl)
      ))

(define simulate)
(if (top-level-bound? 'SWL-ACTIVE)
    (set! simulate graphical-simulation)
    (set! simulate run-simulation))


(define (testem)
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


(define (g) (eval (cadadr testssim)))

(pretty-maximum-lines 2000)
