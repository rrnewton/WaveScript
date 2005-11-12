
(module pass31_flatten-tokmac mzscheme
  (require (lib "include.ss"))
  (require (lib "list.ss"))
;  (require (lib "trace.ss"))

  (require "constants.ss"
	   "hashtab.ss"
	   "iu-match.ss")
  (require (all-except "tml_generic_traverse.ss" test-this these-tests)
           (all-except "grammar_checker.ss" test-this these-tests)
	   (all-except "helpers.ss" filter test-this these-tests))
 
  (include (build-path  "generic" "pass31_flatten-tokmac.ss"))
  
  (provide flatten-tokmac ;test-this
           )
  )

;(require pass31_flatten-tokmac)

#;
(flatten-tokmac
 '(rename-stored-lang
  '(program
     (bindings)
     (nodepgm
       (tokens
         (node-start subtok_ind () (stored) (void))
         (SOC-start
           subtok_ind
           ()
           (stored)
           (call (tok f 0) '1 '2 '3 '4 '5))
         (f subtok_ind
            (a b c d e)
            (stored)
            (let ([socretval_1 (list a b c d e)])
              (if (= (my-id) '10000)
                  (call (tok SOC-return-handler 0) socretval_1)
                  (dbg '"ERROR: soc-return called on node other than base station, node id: %d"
                       (my-id)))))))))
)