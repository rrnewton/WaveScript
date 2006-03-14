(module pass16_add-places mzscheme

  (require (lib "include.ss")
	   (lib "trace.ss")
	   "../generic/constants.ss"
	   "prim_defs.ss"
	   "iu-match.ss"
	   (all-except "helpers.ss" test-this these-tests)
	   (all-except "regiment_helpers.ss" test-this these-tests))

  (include (build-path "generic" "pass16_add-places.ss"))
  
;  (provide deglobalize
;;	   test-this these-tests test13 tests13)

  ;; Should demonstrate point->area with the circle
  ;; unknown->point for the anchor.  
  ;; Alas, it should infer that these places are the same, but it doesn't know that right now.
   (define (test16)
     (add-places '(add-heartbeats-language
                  '(program
                    (props
                     (tmpanch_3 leaf anchor distributed)
                     (tmpunknpr_4 local)
                     (tmp_1 local)
                     (result_2 region area distributed final))
                    (control-flow ????)
                    (lazy-letrec
                     ((tmp_1 #f (cons '30 tmpunknpr_4))
                      (result_2 1.0 (circle tmpanch_3 '50))
                      (tmpunknpr_4 #f (cons '40 '()))
                      (tmpanch_3 1.0 (anchor-at tmp_1)))
                     result_2)))))
     
  (provide (all-defined))
  )

;(require pass14_add-places)


