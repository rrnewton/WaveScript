;; This is crawls a structure to produce a simple hash code.
;; Taken from slib.

(module hash mzscheme
  (provide hash)
  (require (lib "include.ss"))
  
  (include (build-path "generic" "util" "slib_hash.ss"))
  
  )

;(require hash)
