




(define-language
  'add-control-flow-language

  (make-begin
    `(,(annotate-heartbeats-language 'return)
      (define-syntax control-flow
	(syntax-rules ()
	  [(_ . stuff) (void)]))

      )))

