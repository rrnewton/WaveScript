
;; UNDER CONSTRUCTION:


;; [2006.04.24] TODO:
;; Define a new global-simulation that uses the actual sensor stubs to populate the streams.
(define-language
  'base-language

  '(begin
     
     (define world (map cons the-test-field the-test-field))
     (define radius 27.0)

;     (reg:define-struct (baselang-simnode id sensor pos))
     
     (define (entry? r)
       (and (pair? r)
	    (baselang-simnode? (cdr r))))

     (define (region? r)
       (and (list? r)
	    (andmap entry? r)))

     (define (quickdepth r)
       (if (entry? r) 0
	   (add1 (quickdepth (car r)))))

     ))

