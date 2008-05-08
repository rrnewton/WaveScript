
(module identifier-syntax mzscheme
  (provide identifier-syntax 
           ;define-alias
           )
  (require-for-template mzscheme)
  
  (define-syntax identifier-syntax
   (lambda (x)
    (syntax-case x ()
      [(_ e)
       #'(lambda (x)
           (syntax-case x ()
             [id
              (identifier? #'id) 
              #'e]
             [(id x (... ...))
              ;(identifier? #'id)
              #'(#%app e x (... ...))]
             [(#%app id x (... ...))
              #'(#%app e x (... ...))]
             ))])))

  #;
  (define-syntax define-alias
    (syntax-rules ()
      [(_ v e) (define-syntax v (identifier-syntax e))]))

)

