;; This is crawls a structure to produce a simple hash code.

(module hash mzscheme
  (provide hash)
  (require (lib "include.ss"))
  
  (include (build-path "generic" "util" "hash.ss"))
  
  )

;(require hash)
