
;;;; 
;;;; .author Ryan Newton

;;;; [2007.12.27] This loads the bulk of the Regiment/WaveScript
;;;; source code.  It's shared between the different backends.
;;;; Hopefully this is the only place you'll find a long list of all
;;;; the source filenames in the project.


(common:load-source "generic/util/helpers.ss") 

;; These provide some more utility code related to threads:
(cond-expand
 [(and chez threads)
  (printf "Configuring for multithreaded execution.\n")
  (common:load-source "chez/threaded_utils.ss")
  ;; Threads don't get initialized until we run the compiler.
  (import threaded_utils)]
 ;; Otherwise provide a dummy implementation of "par":
 [else 
  (define par list)
  (define parmv values)
  (define par-list (lambda (th*) (map (lambda (th) (th)) th*)))
  (define par-map map)
  (define (init-par cpus) (void))
  (define (par-status) (void))
  (define (par-reset!) (void))
  (define (shutdown-par) (void))])

;; Not using these currently:
;(common:load-source "generic/util/imperative_streams.ss") ;(import (except imperative_streams test-this these-tests))


;; Include these at top-level for the system tests:
(common:load-source "generic/util/streams.ss")

;; Lists all the Regiment primitives and their types:
(common:load-source "generic/compiler_components/prim_defs.ss")

