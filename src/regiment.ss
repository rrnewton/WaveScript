;#! /usr/bin/scheme --script
;#!/usr/bin/chez --script

;; Regiment.ss
;; This file is a script that drives the regiment compiler/simulator.


;(load (string-append (getenv "HOME") "/scheme/chez/full_chez.ss"))


(define (print-types-and-exit prog . opts)
  (define verbose? (memq 'verbose opts))
  (printf ";; Regiment program with infered types: \n")
  ;; Run just the verify regiment pass, it will associate types:
  (parameterize ([print-vector-length #f])
  (match (verify-regiment `(lang '(program ,prog)))
    [(,lang '(program ,p ,t))
     (match p
       [(letrec ([,id* ,t* ,rhs*] ...) ,bod)
	(for-each (lambda (id t rhs)
		    (if verbose?
			(begin (pretty-print `(define ,id : ,t ,rhs))(newline))
			(printf "~a : ~a\n" (pad-width 30 id) t)))
	  id* t* rhs*)
	(if verbose? (pretty-print bod))]
       [,p (pretty-print p)])
     ;(printf "\n;; Regiment program return type: ~a\n" t)
     (printf "  : ~a\n" t)
     (exit)]
    [,other (error 'print-types-and-exit "bad output from verify-regiment: ~s" other)])))
