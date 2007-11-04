#!/usr/bin/mzscheme -C


;
;
;
(module compare mzscheme

  ;
  (require (lib "list.ss")
           "measure.ss")

  ;
  (provide compare)


;
;
;
(define (compare meta-plan)

  (define static-params            (filter (lambda (pair) (not (eq? 'var (car pair)))) meta-plan))
  (define variable-params (map cdr (filter (lambda (pair)      (eq? 'var (car pair)))  meta-plan)))
  
  ; FIXME: misleading name; not a product of its two args.
  (define (product var-plan fixed-plan)
    (if (null? var-plan)
        `(,fixed-plan)
        (apply append (map (lambda (v) (product (cdr var-plan) (cons `(,(caar var-plan) . ,v) fixed-plan)))
                           (cadar var-plan)))))

  (map measure-wavescript-program (product variable-params static-params)))


;
; end of module declaration
;
)
