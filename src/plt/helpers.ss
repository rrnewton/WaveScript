
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
  
  (provide 
     
   unique-name reset-name-count! extract-suffix
   code-name label-name #;method-name
   
   constant? datum? formalexp? cast-formals default-unit-tester
   
   regiment-primitives regiment-primitive? 
  
   set? list->set set-cons union intersection difference
   list-head filter list-index snoc rac rdc 
   insert-between iota disp
      
   (all-from (lib "rutils_generic.ss") )
   ;   (all-from-except (lib "rutils_generic.ss") 
   ;                    list->set union intersection difference set?) 
   )
  )


