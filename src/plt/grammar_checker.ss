;; [2005.09.26]

#cs ;; Case Sensitivity

(module grammar_checker mzscheme
  (require (lib "include.ss"))
  (require "iu-match.ss")
  (require "constants.ss") ;; For DEBUGMODE
  (require (all-except "helpers.ss" test-this these-tests))
  
  (define (atom? x) (or (symbol? x) (number? x) (null? x) (boolean? x) (char? x)))
  
  (provide 
         check-grammar
         build-compiler-pass
         
         ;; Predifined Grammars
         basic_tml_grammar
         tml_gradient_grammar
         tml_letstored_grammar
         full_but_clean_tml
         
         these-tests test-this
         test-grammar tests-grammar
         )
  
  (include (build-path "generic" "grammar_checker.ss"))
)
