;;;; This imitates Chez's make-engine facility using coroutines from thread.ss
;;;; .author Ryan Newton.
;;;;

(module engine mzscheme
  (require (lib "thread.ss"))
  (provide make-engine engine-return engine-block)

  ;; Warning, this is not fully compatible. 
  (define (make-engine th)
    (let ([co (coroutine (lambda (_) (th)))])      
      (let loop ([invoked #f])
        ;; This closure is only invokable once:
        (lambda (ticks succ fail)
	  ;; Adjust:
	  ;(set! ticks (quotient ticks 10))
          (if invoked 
              (error 'make-engine
                     "This PLT hack for engines is not reentrant.  Don't call the same engine twice."))
          (set! invoked #t)
          (let ([start-time (current-inexact-milliseconds)])
            (if (coroutine-run ticks co)
                ;; Approximate the number of "ticks left":
                (succ (max 0 (- ticks (- (current-inexact-milliseconds) start-time)))
                      (coroutine-result co))
                ;; Return a new closure that will allow you to run it one more iteration:
                (fail (loop #f))))))))
  
  (define (engine-return . args) (error 'engine-return "not implemented in PLT"))
  (define (engine-block . args) (error 'engine-block "not implemented in PLT"))
  
  )

;(require engine)
;(define (loop) (loop))
;(define e (make-engine loop))
