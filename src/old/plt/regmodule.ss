

(module regmodule mzscheme
  (require)
  (provide  chezimports 
	    chezprovide)

  ;; These stubs allow our common module syntax to work.
  (define-syntax chezimports
    (syntax-rules ()
      [(_ e ...) (begin)]))
  (define-syntax chezprovide
    (syntax-rules ()
      [(_ e ...) (begin)]))     

)
