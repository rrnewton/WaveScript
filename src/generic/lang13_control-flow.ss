


(define-language
  'add-control-flow-language

  (make-begin
    `(,(add-heartbeats-language 'return)
      (define-syntax control-flow
	(syntax-rules ()
	  [(_ . stuff) (void)]))

      )))

