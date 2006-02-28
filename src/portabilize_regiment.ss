

;;;; Experimental.  Desugars the whole Regiment source into a much more portable (syntax expanded) single file.

;;;; To use this, first switch reg:define-struct to use vectors rather than

(define regiment (expand-file "compiler_chez.ss"))

