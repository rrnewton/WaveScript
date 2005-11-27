;;;; constants.ss
;;;; These are the chez specific constant/datatype definitions.
;======================================================================


;; Could be define-structure or define-record.   <br> 
;; This defines the record-representation used throughout the regiment code.
(define-syntax reg:define-struct
  (syntax-rules ()
    [(_ (name field ...))  (define-record name (field ...))]))
;   [(_ (name field ...))  (define-structure (name field ...))]))

(include "generic/constants.ss")
