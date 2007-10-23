

;; Take heavy stateless operators and split them N ways.

(define (stateless? iterate)
  #t)


;; Produce code for a splitter.

;; Since we don't directly support multiple outputs from an iterate,
;; It's best to actually produce several iterates ("filters") in front
;; of each of the individual operators.
(define (splitter nways)
  (define cnt (unique-name 'cnt))
  (define elm (unique-name 'elm))
  (define vq (unique-name 'vq))
  
  `(iterate (letrec ([,cnt Int '0])
	      (lambda (,elm ,vq) ('a (VQueue 'a))
		      
		      )
	      ))
  

  )

;; This produces a single iterate that joins
(define (joiner )
  
  )