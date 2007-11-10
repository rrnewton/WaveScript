#!/usr/bin/mzscheme -C


;
;
;
(module compare mzscheme

  ;
  (require (lib "list.ss")
           "compile.ss"
           "measure.ss")

  ;
  (provide compare)


;
; actions: list containg the symbols compile, run (either or both)
;
; "compare" is a bit of a misnomer now, with the actions arg: there is no "comparison" if we're just compiling;
; we just want to compile with multiple plans
;
(define (compare meta-plan . actions)

  (define static-params            (filter (lambda (pair) (not (memq (car pair) '(var mvar)))) meta-plan))
  (define variable-params (map cdr (filter (lambda (pair)      (eq? 'var  (car pair)))         meta-plan)))
  (define multivar-params (map cdr (filter (lambda (pair)      (eq? 'mvar (car pair)))         meta-plan)))

  ; FIXME: misleading name; not a product of its two args.
  (define (product var-plan fixed-plan)
    (if (null? var-plan)
        `(,fixed-plan)
        (apply append (map (lambda (val) (product (cdr var-plan)
                                                  (if (pair? (caar var-plan)) ;; indicates mvar vs. var
                                                      (append (map cons (caar var-plan) val) fixed-plan)
                                                      (cons `(,(caar var-plan) . ,val) fixed-plan))
                                                  ))
                           (cadar var-plan)))))

  (map (cond [(and (memq 'compile actions) (memq 'run actions))
              compile-and-measure-wavescript-program]
             [(memq 'compile actions)
              compile-wavescript-program]
             [(memq 'run actions)
              measure-wavescript-program]
             [else (error 'compare "please, specify one or both of 'compile, 'run")])

    (product (append multivar-params variable-params) static-params))

  #;
  (map (if compile? compile-and-measure-wavescript-program measure-wavescript-program)
    (product (append multivar-params variable-params) static-params))
  )


;
; end of module declaration
;
)
