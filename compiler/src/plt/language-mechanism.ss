
(module language-mechanism mzscheme  
  (require (lib "include.ss")           
	   (lib "pretty.ss")
           (all-except (lib "compat.ss") flush-output-port)
           "iu-match.ss"
           "helpers.ss"
           (lib "27.s" "srfi") ;; Random numbers
	   )

  (define make-n-list
    (lambda (n func)
      (letrec ((loop (lambda (n acc)
                       (if (zero? n)
                           acc
                           (loop (sub1 n) (cons (func n) acc))))))
        (loop n '()))))
  
  (include (build-path ".." "generic" "language-mechanism.ss"))

  ;; This is lame, but I'm doing it for PLTs module system.
  ;(define base-language (eval 'base-language))
  
  (provide (all-defined) (all-from (lib "compat.ss")))
  )

