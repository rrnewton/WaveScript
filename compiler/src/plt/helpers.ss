
(module helpers mzscheme        
  (require (lib "iu-match.ss"))
  (require (lib "include.ss"))
  
  ;; This might not be necessary: 
  ; (require "~/scheme/plt/utils/rutils_generic.ss")
  (require (all-except (lib "rutils_generic.ss")
                       list->set union intersection difference set?
                       list-head filter list-index snoc rac rdc 
                       insert-between iota disp))

  (include (build-path ".." "generic" "helpers.ss"))
  
  (define (define-top-level-value sym obj)
    (eval `(define ,sym ,obj)))
  (define (set-top-level-value! sym obj)
    (eval `(set! ,sym ,obj)))
  (define (top-level-value sym) (eval sym))
  (define (top-level-bound? sym)
    (error 'top-level-bound?
	   "This is a chez function which can't ~a"
	   "be emulated right now in Plt. -RRN"))

  (define flush-output-port flush-output)

  (define-syntax rec
    (syntax-rules ()
      ((_ x e) (letrec ((x e)) x))))
  
  (define-syntax mvlet
    (syntax-rules ()
        [(mvlet stuff ...) (let-values stuff ...)]))
  
  (provide 

   ;; Syntax:
   mvlet rec
   
   ;; Values:
   define-top-level-value set-top-level-value! top-level-bound? top-level-value
   flush-output-port
   
   
   get-formals
   unique-name reset-name-count! extract-suffix make-begin
   code-name label-name #;method-name
   
   ;; Hmm, not sure what meaning immediate has here..
   immediate? constant? datum? formalexp? cast-formals default-unit-tester
   
   regiment-primitives regiment-primitive? 

   set? list->set set-cons union intersection difference
   list-head filter list-index snoc rac rdc 
   insert-between iota disp
   
   
;   (all-except (lib "rutils_generic.ss")
;               list->set union intersection difference set?
;               list-head filter list-index snoc rac rdc 
;               insert-between iota disp)
   (all-from (lib "rutils_generic.ss") )
   ;   (all-from-except (lib "rutils_generic.ss") 
   ;                    list->set union intersection difference set?) 
   )
  
  )


