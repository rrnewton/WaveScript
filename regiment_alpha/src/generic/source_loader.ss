;; source_loader.ss 

;; [2005.11.11] This module loads regiment source from a ".rs" file
;; and desugars it according to a few simple conventions that I've
;; followed.

(define read-regiment-source-file
  (lambda (fn)

    (match (file->slist fn)
      [((parameters ,p ...) ,prog)
       (values prog p)]
      
      [,prog (values prog ())]

      )))
