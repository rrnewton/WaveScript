#!/usr/bin/mzscheme -C

;
; FIXME: important caveat: pass filename with ./ at the front!
;        this will be fixed soon!
;


; FIXME: need framework for env. vars.!
; FIXME: also want to compare results of outputs?
;

;
;
;
(require "running.ss")



;
;
;
(define (main args)

  (if (< (length args) 4)
      (begin (print-usage (car args)) (exit 1)))

  (let ((prog-opts-on  (compile-program (cadr args) "opts-on" ()))
        (prog-opts-off (compile-program (cadr args) "opts-off" (map string->symbol (cdddr args))))
        (num-runs (string->number (caddr args))))
    
    ;
    ; interleave the runs
    ;
    (let ((results
           (n-times (lambda (x y)
                      (list (measure-wavescope-program x) (measure-wavescope-program y)))
                    num-runs prog-opts-off prog-opts-on)))

      (printf "optimizations on:  (<processing time> <user time> <system time>)~n")
      (display (foldr (lambda (x y) (map + x y)) '(0 0 0) (map car results)))
      (newline)

      (printf "optimizations off: (<processing time> <user time> <system time>)~n")
      (display (foldr (lambda (x y) (map + x y)) '(0 0 0) (map cadr results)))
      (newline))
    
    ;
    ; don't interleave
    ;
    #;
    (begin
      (printf "running ~a:~n" prog-opts-off)
      (display (foldr (lambda (x y) (map + x y)) '(0 0 0) (n-times measure-wavescope-program num-runs prog-opts-off)))
      (newline)

      (printf "running ~a:~n" prog-opts-on)
      (display (foldr (lambda (x y) (map + x y)) '(0 0 0) (n-times measure-wavescope-program num-runs prog-opts-on)))
      (newline))

    ))


;
;
;
(define (print-usage script-name)
  (printf "Usage:~n")
  (printf "~a <file> <num. runs> [optimizations ...]~n" script-name))