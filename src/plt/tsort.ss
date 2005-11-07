;; [2004.06.02] RRN:
;; This is lifted straight from slib3a1.
;; I hacked it to use mzscheme's hashtables.


(module tsort mzscheme
;;  (require "helpers.ss")
  (require (lib "pretty.ss")
	   (lib "include.ss")
;           (lib "list.ss")
           (all-except "helpers.ss" test-this these-tests)
           (all-except "hashtab.ss" test-this these-tests))
  (provide tsort topological-sort  cyclic?
	   test-this these-tests test-tsort)

  (include (build-path "generic" "tsort.ss"))
  
)
